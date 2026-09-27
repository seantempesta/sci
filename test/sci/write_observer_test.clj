(ns sci.write-observer-test
  "The execution-scoped producer observers: every completed root write once,
  outside retries and before watches, with its source occurrence."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.lang]))

(defn- observed
  "Evaluate `src` in `ctx` (read with end locations) under one write observer;
  answer [value events]."
  [ctx src]
  (let [events (atom [])
        rdr (sci/source-reader src)
        value (binding [sci.lang/*write-observer*
                        (fn [e] (swap! events conj
                                       (cond-> (dissoc e :var)
                                         (:var e) (assoc :sym (let [m (meta (:var e))]
                                                                (when (:name m)
                                                                  (symbol (str (:ns m)) (str (:name m)))))
                                                         :v (:var e)))))]
                (loop [ret nil]
                  (let [f (sci/parse-next ctx rdr {:end-location true})]
                    (if (= ::sci/eof f) ret (recur (sci/eval-form ctx f))))))]
    [value @events]))

(defn- binds [events] (filterv #(= :bind (:op %)) events))

(deftest every-root-write-is-observed-once
  (let [ctx (sci/init {})]
    (sci/eval-string* ctx "(def f 1)")
    (let [[v es] (observed ctx "(def f 2)\n(intern 'user 'f 3)\n(alter-var-root #'f inc)\nf")]
      (is (= 4 v))
      (is (= [2 3 4] (mapv :root (binds es))))
      (is (= [{:line 1 :column 1 :end-line 1 :end-column 10} nil nil] (mapv :origin (binds es))))))
  (testing "intern of a new Var, defonce twice, declare"
    (let [[_ es] (observed (sci/init {}) "(intern 'user 'g 5)\n(defonce o 1)\n(defonce o 2)\n(declare d)")]
      (is (= '[user/g user/o] (mapv :sym (binds es))))))
  (testing "nested eval"
    (let [[v es] (observed (sci/init {}) "(do (eval '(def h 1)) h)")]
      (is (= 1 v))
      (is (= '[user/h] (mapv :sym (binds es))))))
  (testing "unmap and remove-ns are events"
    (let [[_ es] (observed (sci/init {}) "(def u 1)\n(ns-unmap *ns* 'u)\n(create-ns 'gone)\n(remove-ns 'gone)")]
      (is (= [:bind :unmap :remove-ns] (mapv :op es)))))
  (testing "letfn's local Vars are written but belong to no namespace"
    (let [[v es] (observed (sci/init {}) "(letfn [(a [] 1)] (a))")]
      (is (= 1 v))
      (is (seq (binds es)))
      (is (every? #(nil? (:ns (meta (:v %)))) (binds es))))))

(deftest the-observed-var-is-the-published-var
  (testing "an inherited redefinition: one event, on the Var the fork resolves"
    (let [base (sci/init {})
          _ (sci/eval-string* base "(def f (fn [] 1))")
          ctx (sci/fork base)
          seen (atom [])]
      (binding [sci.lang/*write-observer*
                (fn [e]
                  (when (= :bind (:op e))
                    (let [v (:var e)]
                      (swap! seen conj {:same-var? (identical? v (sci/resolve ctx 'user/f))
                                        :observed ((:root e))})
                      (binding [sci.lang/*write-observer* nil]
                        (sci/bind-root! ctx v (let [f (:root e)] (fn [] (+ 10 (f)))))))))]
        (is (= 12 (sci/eval-string* ctx "(def f (fn [] 2)) (f)"))))
      (is (= [{:same-var? true :observed 2}] @seen))
      (is (= 1 (sci/eval-string* base "(f)")))))
  (testing "a callback that defines does not repeat the outer write"
    (let [ctx (sci/init {}) seen (atom []) once (atom false)]
      (binding [sci.lang/*write-observer*
                (fn [e]
                  (swap! seen conj (str (:name (meta (:var e)))))
                  (when (compare-and-set! once false true)
                    (binding [sci.lang/*write-observer* nil]
                      (sci/eval-string* ctx "(def nested 9)"))))]
        (sci/eval-string* ctx "(def outer 1)"))
      (is (= ["outer"] @seen))
      (is (= 9 (sci/eval-string* ctx "nested")))))
  (testing "a failing watch cannot hide a completed write"
    (let [ctx (sci/init {}) seen (atom [])]
      (sci/eval-string* ctx "(def w 1) (add-watch #'w :bad (fn [& _] (throw (ex-info \"watch failed\" {}))))")
      (is (= "watch failed"
             (binding [sci.lang/*write-observer* #(swap! seen conj (select-keys % [:op :root]))]
               (try (sci/eval-string* ctx "(alter-var-root #'w inc)")
                    (catch Throwable t (ex-message t))))))
      (is (= 2 (sci/eval-string* ctx "w")))
      (is (= [{:op :bind :root 2}] @seen))))
  (testing "a fork's write to an inherited Var leaves the base unchanged"
    (let [base (sci/init {})
          _ (sci/eval-string* base "(def k 1)")
          fork (sci/fork base)
          [v es] (observed fork "(alter-var-root #'k inc)\nk")]
      (is (= 2 v))
      (is (= '[user/k] (mapv :sym (binds es))))
      (is (= 1 (sci/eval-string* base "k"))))))

(deftest a-definition-carries-its-source-occurrence
  (testing "a literal def is its own occurrence"
    (let [[_ es] (observed (sci/init {}) "(def a 1)")]
      (is (= [{:line 1 :column 1 :end-line 1 :end-column 10}] (mapv :origin es)))))
  (testing "a macro's single def is the macro call's occurrence, not authored"
    (let [[v es] (observed (sci/init {}) "(defmacro single [] (list 'def 'made 7))\n(single)\nmade")]
      (is (= 7 v))
      (is (= {:line 2 :column 1 :end-line 2 :end-column 9 :expanded-by 'single}
             (:origin (last (binds es)))))))
  (testing "defs generated inside a let by an agent macro"
    (let [[_ es] (observed (sci/init {}) "(defmacro pair [] `(do (def ~'px 1) (def ~'py 2)))\n(let [z 0]\n  (pair))")]
      (is (= [{:line 3 :column 3 :end-line 3 :end-column 9 :expanded-by 'pair}
              {:line 3 :column 3 :end-line 3 :end-column 9 :expanded-by 'pair}]
             (mapv :origin (rest (binds es)))))))
  (testing "a top-level do expansion keeps its call"
    (let [[_ es] (observed (sci/init {}) "(defmacro two [] `(do (def ~'ta 1) (def ~'tb 2)))\n(two)")]
      (is (= [{:line 2 :column 1 :end-line 2 :end-column 6 :expanded-by 'two}
              {:line 2 :column 1 :end-line 2 :end-column 6 :expanded-by 'two}]
             (mapv :origin (rest (binds es)))))))
  (testing "a def a macro passes through (let, when, a wrapper) keeps its own span"
    (let [[_ es] (observed (sci/init {}) "(defmacro wrap [& b] `(do ~@b))\n(let [u 0] (def lv 1))\n(when true (def wv 1))\n(wrap (def mv 1))")]
      (is (= [{:line 2 :column 12 :end-line 2 :end-column 22}
              {:line 3 :column 12 :end-line 3 :end-column 22}
              {:line 4 :column 7 :end-line 4 :end-column 17}]
             (mapv :origin (rest (binds es)))))))
  (testing "defn is the call it was written as"
    (let [[_ es] (observed (sci/init {}) "(let [k 1]\n  (defn nd [x] (+ k x)))")]
      (is (= {:line 2 :column 3 :end-line 2 :end-column 24 :expanded-by 'defn}
             (:origin (first (binds es))))))))

(deftest extensions-and-members-are-marked-at-their-producer
  (let [[_ es] (observed (sci/init {}) "(defprotocol P (pm [x]))\n(extend-protocol P String (pm [x] 1))\n(defrecord R [a] P (pm [_] a))")
        by-sym (group-by :sym (binds es))]
    (testing "the protocol is one definition; extensions are :extend writes"
      (is (= [:root :root :extend :extend] (mapv :kind (get by-sym 'user/P))))
      (is (= 1 (count (:methods (:root (second (get by-sym 'user/P))))))))
    (testing "the record constructor helper names its member"
      (is (= '->R (some #(:sci.impl/member-of (meta (:v %))) (binds es))))))
  (testing "the expansion observer sees each macro call once"
    (let [seen (atom [])]
      (binding [sci.lang/*expansion-observer* (fn [call exp] (swap! seen conj [(first call) exp]))]
        (sci/eval-string "(defmacro single [] (list 'def 'made 7)) (single)"))
      (is (= '(def made 7) (some (fn [[op exp]] (when (= 'single op) exp)) @seen))))))
