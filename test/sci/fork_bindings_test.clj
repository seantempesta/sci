(ns sci.fork-bindings-test
  "A Var keeps its identity across forks; each context reads its own binding
  of it, so code analyzed in a base and run in a fork sees the fork's
  redefinitions and the base never sees them."
  (:require [clojure.test :as t :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.ctx-store :as store]))

(defn- ev [ctx s] (sci/eval-string* ctx s))

(defn- test-ctx
  "A context with clojure.test and a note collector, so a deftest's body
  reports which callee it ran."
  [notes]
  (sci/init {:namespaces {'clojure.test (sci/copy-ns clojure.test (sci/create-ns 'clojure.test))
                          'user {'note (fn [x] (swap! notes conj x) x)}}}))

(defn- run-test-var
  "Runs the deftest Var named sym through clojure.test/test-var in ctx;
  returns the notes its body made."
  [ctx notes sym]
  (reset! notes [])
  (binding [t/*report-counters* (ref t/*initial-report-counters*)
            t/*test-out* (java.io.StringWriter.)]
    (store/with-ctx ctx (t/test-var (sci/resolve ctx sym))))
  @notes)

(deftest an-inherited-test-sees-the-fork-redefinition-of-its-callee
  (let [notes (atom [])
        base (test-ctx notes)
        _ (ev base "(require '[clojure.test :refer [deftest]])
                    (defn f [x] (note [:base x]))
                    (defn checker [] (f 1))
                    (deftest f-test (f 2))")
        redefined (sci/fork base)
        rearmed (sci/fork base)]
    (ev redefined "(defn f [x] (note [:fork x]))")
    (let [f-var (sci/resolve rearmed 'f)
          armed (fn [x] (swap! notes conj [:armed x]) [:armed x])]
      (is (identical? f-var (sci/bind-root! rearmed f-var armed))
          "re-arming returns the same Var"))
    (testing "an inherited caller"
      (is (= [:fork 1] (ev redefined "(checker)")))
      (is (= [:armed 1] (ev rearmed "(checker)")))
      (is (= [:base 1] (ev base "(checker)"))))
    (testing "an inherited deftest, called and run by clojure.test"
      (reset! notes [])
      (ev redefined "(f-test)")
      (is (= [[:fork 2]] @notes))
      (is (= [[:fork 2]] (run-test-var redefined notes 'f-test)))
      (is (= [[:armed 2]] (run-test-var rearmed notes 'f-test)))
      (is (= [[:base 2]] (run-test-var base notes 'f-test))))
    (is (identical? (sci/resolve base 'f) (sci/resolve redefined 'f))
        "no context copies the Var")))

(deftest a-definition-re-evaluated-in-a-fork-names-its-own-var
  (let [notes (atom [])
        base (test-ctx notes)
        _ (ev base "(require '[clojure.test :refer [deftest]])
                    (defn self [] #'self)
                    (deftest check (note :base))")
        child (sci/fork base)]
    (ev child "(defn self [] [:new #'self]) (deftest check (note :child))")
    (reset! notes [])
    (ev child "(check)")
    (is (= [:child] @notes) "the new body dispatches through its own Var")
    (is (= [:child] (run-test-var child notes 'check)))
    (is (true? (ev child "(= [:new #'self] (self))")))
    (is (= [:base] (run-test-var base notes 'check)))))

(deftest references-follow-the-executing-context
  (let [base (sci/init {})
        _ (ev base "(defn g [] :base)
                    (defn h [] (g))
                    (defn value [] g)
                    (defn handle [] #'g)
                    (def captured g)
                    (def x 1)
                    (defn rx [] x)
                    (def closure (let [y 10] (fn [] [(g) y])))")
        child (sci/fork base)
        sibling (sci/fork base)]
    (ev child "(defn g [] :child) (def x 2)")
    (is (= [:child :child :child :child 2 [:child 10]]
           (ev child "[(g) (h) ((value)) ((handle)) (rx) (closure)]")))
    (is (= :base (ev child "(captured)"))
        "a function value captured before the redefinition stays that value")
    (is (= [:base :base 1] (ev base "[(g) (h) (rx)]")))
    (is (= [:base :base 1] (ev sibling "[(g) (h) (rx)]")))
    (testing "a grandchild inherits its parent's binding"
      (let [grandchild (sci/fork child)]
        (is (= [:child 2] (ev grandchild "[(h) (rx)]")))
        (ev grandchild "(defn g [] :grandchild)")
        (is (= :grandchild (ev grandchild "(h)")))
        (is (= :child (ev child "(h)")))))))

(deftest a-fork-is-a-snapshot-of-both-sides
  (let [parent (sci/init {})
        _ (ev parent "(defn g [] :before) (defn h [] (g)) (def n 0)")
        child (sci/fork parent)]
    (ev parent "(defn g [] :parent-after-fork) (alter-var-root #'n inc)")
    (is (= [:before 0] (ev child "[(h) n]")) "a parent's later writes never reach the fork")
    (is (= [:parent-after-fork 1] (ev parent "[(h) n]")))
    (ev child "(alter-var-root #'n + 10)")
    (is (= 10 (ev child "n")))
    (is (= 1 (ev parent "n")))))

(deftest unmapping-and-reinterning-mints-a-new-var
  (let [base (sci/init {})
        _ (ev base "(defn g [] :old) (defn h [] (g)) (def old-var #'g)")
        child (sci/fork base)]
    (is (= [:new :old :old false]
           (ev child "(ns-unmap *ns* 'g) (defn g [] :new)
                      [(g) (h) (old-var) (identical? old-var #'g)]")))
    (is (= :old (ev base "(g)")))))

(deftest a-declared-var-defined-in-a-fork
  (let [base (sci/init {})
        _ (ev base "(declare q) (defn r [] (q))")
        child (sci/fork base)]
    (ev child "(defn q [] 7)")
    (is (= 7 (ev child "(r)")))
    (is (true? (ev child "(bound? #'q)")))
    (is (false? (ev base "(bound? #'q)")))))

(deftest dynamic-bindings-are-unaffected
  (let [base (sci/init {})
        _ (ev base "(def ^:dynamic *d* :base) (defn rd [] *d*)")
        child (sci/fork base)]
    (ev child "(def ^:dynamic *d* :fork)")
    (is (= [:fork :bound :fork] (ev child "[(rd) (binding [*d* :bound] (rd)) (rd)]")))
    (is (= [:base :bound :base] (ev base "[(rd) (binding [*d* :bound] (rd)) (rd)]")))))

(deftest metadata-is-per-context
  (let [base (sci/init {})
        _ (ev base "(defn f \"Base.\" [] :f)")
        child (sci/fork base)]
    (ev child "(alter-meta! #'f assoc :doc \"Child.\")")
    (is (= "Child." (ev child "(:doc (meta #'f))")))
    (is (= "Base." (ev base "(:doc (meta #'f))")))
    (is (= "Child." (store/with-ctx child (:doc (meta (sci/resolve child 'f)))))
        "a host read inside the context sees the context's binding")
    (is (= "Base." (:doc (meta (sci/resolve child 'f))))
        "a host read with no context sees the Var's own binding")))

(deftest concurrent-siblings-read-their-own-bindings
  (let [base (sci/init {})
        _ (ev base "(defn g [] :base) (defn h [n] (loop [i 0 acc #{}] (if (< i n) (recur (inc i) (conj acc (g))) acc)))")
        forks (mapv (fn [k] (doto (sci/fork base) (ev (str "(defn g [] " k ")")))) [:a :b])
        results (mapv (fn [ctx] (future (ev ctx "(h 10000)"))) forks)]
    (is (= [#{:a} #{:b}] (mapv deref results)))
    (is (= #{:base} (ev base "(h 10)")))))

(deftest host-invocation-reads-the-bound-context
  (let [base (sci/init {})
        _ (ev base "(defn g [] :base) (defn h [] (g))")
        child (sci/fork base)
        _ (ev child "(defn g [] :child)")
        h (ev child "h")]
    (is (= :child (store/with-ctx child (h))))
    (is (= :child (h)) "read on the child, called with no context: the child")
    (is (= :base ((ev base "h"))) "read on the base, called with no context: the base")))

(defn- host-ctx
  "A context whose host namespace runs an SCI fn on a raw Thread, an executor
  and a binding-conveying future."
  []
  (let [on-thread (fn [f] (let [p (promise)]
                            (.start (Thread. ^Runnable (fn [] (deliver p (f)))))
                            (deref p 2000 :timeout)))]
    (sci/init {:namespaces
               {'host {'thread on-thread
                       'pool (fn [f] (.get (.submit (java.util.concurrent.ForkJoinPool/commonPool)
                                                    ^Callable f)))
                       'future (fn [f] (deref (future-call f) 2000 :timeout))}}})))

(deftest a-context-less-thread-runs-an-sci-fn-under-its-defining-context
  ;; A host thread SCI did not enter has no *ctx*; Clojure's Var roots are
  ;; global, so a call there must read the current definitions, not each
  ;; Var's first binding.
  (let [base (host-ctx)
        _ (ev base "(def holder 1) (defn priv [] :first) (defn calls-priv [] [holder (priv)])")
        fork (sci/fork base)
        _ (ev fork "(def holder 2) (defn priv [] :second)")
        grandchild (sci/fork fork)
        _ (ev grandchild "(defn priv [] :third)")]
    (testing "raw Thread, executor and future from a fork see the fork"
      (is (= [[2 :second] [2 :second] [2 :second]]
             (ev fork "[(host/thread (fn [] [holder (priv)]))
                        (host/pool (fn [] [holder (priv)]))
                        (host/future (fn [] [holder (priv)]))]"))))
    (testing "an inherited caller nested in a fork's fn sees the fork on any thread"
      (is (= [[2 :second] [2 :third]]
             [(ev fork "(host/thread (fn [] (calls-priv)))")
              (ev grandchild "(host/pool (fn [] (calls-priv)))")])))
    (testing "host Var operations and writes on a context-less thread"
      (is (= [:second 2] (ev fork "(host/thread (fn [] [(#'priv) @#'holder]))")))
      (is (= 3 (ev fork "(host/thread (fn [] (alter-var-root #'holder inc) holder))")))
      (is (= 3 (ev fork "holder"))))
    (testing "the base still sees its own"
      (is (= [[1 :first] [1 :first] 1]
             [(ev base "(host/thread (fn [] [holder (priv)]))")
              (ev base "(host/pool calls-priv)")
              (ev base "holder")])))
    (testing "an fn the host holds is called with no context at all"
      (let [g (ev fork "(fn [] [holder (priv)])")]
        (is (= [3 :second] (g)))))
    (testing "a base fn read by value on a fork runs under the fork"
      (is (= [3 :second] (ev fork "(host/thread calls-priv)"))))
    (testing "a context a library stored as the root (reset-ctx!) is no thread's own context"
      ;; clj-kondo's hooks namespace stores its context with reset-ctx! when
      ;; loaded, so every thread SCI did not enter reads it as *ctx*
      (let [stored (.getRawRoot #'store/*ctx*)]
        (try
          (store/reset-ctx! (sci/init {}))
          (is (= [[3 :second] [3 :second]]
                 (ev fork "[(host/thread (fn [] [holder (priv)]))
                            (host/pool (fn [] [holder (priv)]))]")))
          (finally (alter-var-root #'store/*ctx* (constantly stored))))))))

(deftest a-function-escaping-a-fork-resolves-in-that-fork
  ;; A function value made or read while a fork executes and called later on a
  ;; thread with no context (a lazy seq realized after the evaluation, a
  ;; callback a host executor runs) resolves the fork's definitions, though the
  ;; function was analysed in the base.
  (let [base (host-ctx)
        _ (ev base "(defn- hub [x] (inc x))
                    (defn answer [x] (hub x))
                    (defn mapper [xs] (map answer xs))
                    (defn mapper2 [xs] (map #(answer %) xs))")
        branch (sci/fork base)
        _ (ev branch "(defn- hub [x] (+ x 1000))")]
    (testing "a lazy seq over a base function, built on the branch, realized outside"
      (is (= [1001 1002 1003] (vec (ev branch "(mapper [1 2 3])")))))
    (testing "a lambda made on the branch inside a base function, realized outside"
      (is (= [1001 1002 1003] (vec (ev branch "(mapper2 [1 2 3])")))))
    (testing "a base function read on the branch, called on a raw Thread and by the host"
      (let [f (ev branch "answer")
            p (promise)]
        (.start (Thread. ^Runnable (fn [] (deliver p (f 1)))))
        (is (= [1001 1001] [(deref p 2000 :timeout) (f 1)]))))
    (testing "a context bound on the calling thread still wins"
      (is (= 2 (store/with-ctx base ((ev branch "answer") 1)))))
    (testing "the base keeps its own"
      (is (= [2 3 4] (vec (ev base "(mapper [1 2 3])"))))
      (is (= 2 ((ev base "answer") 1))))
    (testing "one read per context answers one function"
      (is (true? (ev branch "(identical? answer answer)")))
      (is (true? (ev branch "(= #{answer} #{answer})"))))))

(deftest a-fork-of-a-frozen-context-writes-nothing
  (let [builder (sci/init {})
        _ (ev builder "(defn g [] :value) (defn h [] (g)) (def n 0) (def ^:dynamic *d* :value)")
        value (sci/freeze builder)
        held (:env value)
        a (sci/fork value)
        b (sci/fork value)]
    (is (map? held) "the value's env is the held map")
    (testing "evaluating against the value itself is refused by its type"
      (is (thrown? ClassCastException (ev value "(def n 1)"))))
    (ev a "(defn g [] :a) (alter-var-root #'n inc) (def private (atom 1)) (alter-meta! #'h assoc :doc \"A.\")")
    (ev b "(ns-unmap *ns* 'h) (defn g [] :b)")
    (is (identical? held (:env value)) "forking and the forks' writes leave the value untouched")
    (is (= [:a :a 1 "A." true] (ev a "[(g) (h) n (:doc (meta #'h)) (some? (resolve 'private))]")))
    (is (= [:b nil 0] (ev b "[(g) (resolve 'h) n]")) "a sibling sees none of a's writes")
    (testing "a new fork of the value sees the value, none of its forks"
      (is (= [:value :value 0 nil nil] (ev (sci/fork value) "[(g) (h) n (:doc (meta #'h)) (resolve 'private)]"))))
    (testing "the builder's later writes never reach the value"
      (ev builder "(defn g [] :builder-after)")
      (is (= :value (ev (sci/fork value) "(h)"))))
    (testing "a fork of a live context still renews its source's generation"
      (let [live (sci/init {})
            before (:sci/generation @(:env live))]
        (sci/fork live)
        (is (not= before (:sci/generation @(:env live))))))
    (testing "a fork of the value costs under 1 µs and 3 KB"
      (dotimes [_ 20000] (sci/fork value))
      (let [n 100000
            ^com.sun.management.ThreadMXBean mx (java.lang.management.ManagementFactory/getThreadMXBean)
            thread (.getId (Thread/currentThread))
            b0 (.getThreadAllocatedBytes mx thread)
            t0 (System/nanoTime)
            _ (dotimes [_ n] (sci/fork value))
            ns-per (/ (- (System/nanoTime) t0) (double n))
            bytes-per (/ (- (.getThreadAllocatedBytes mx thread) b0) (double n))]
        (is (< ns-per 1000.0) "a fork takes under a microsecond")
        (is (< bytes-per 3072.0) "a fork allocates under 3 KB")))))

(defn- in-ctx [ctx s] (store/with-ctx ctx (sci/eval-string* ctx s)))

(deftest a-method-written-in-a-fork-is-its-own
  (testing "defmethod"
    (let [base (sci/init {})
          _ (in-ctx base "(ns r) (defmulti mm identity) (defmethod mm :a [_] 1)")
          a (sci/fork base) b (sci/fork base)]
      (in-ctx a "(in-ns 'r) (defmethod mm :a [_] 2)")
      (is (= [2 1 1] (mapv #(in-ctx % "(r/mm :a)") [a b base])))))
  (testing "a protocol extension of an inherited record"
    (let [base (sci/init {})
          _ (in-ctx base "(ns p) (defprotocol P (m [this])) (defrecord R [] P (m [_] 1)) (def r (->R))")
          a (sci/fork base) b (sci/fork base)]
      (in-ctx a "(in-ns 'p) (extend-type R P (m [_] 2))")
      (is (= [2 1 1] (mapv #(in-ctx % "(p/m p/r)") [a b base])))))
  (testing "extend-protocol, and satisfies? of the new type"
    (let [base (sci/init {})
          _ (in-ctx base "(ns q) (defprotocol P (m [this])) (extend-protocol P String (m [_] 1))")
          a (sci/fork base) b (sci/fork base)]
      (in-ctx a "(in-ns 'q) (extend-protocol P String (m [_] 2)) (extend-protocol P nil (m [_] 5))")
      (is (= [2 1 1] (mapv #(in-ctx % "(q/m \"x\")") [a b base])))
      (is (= [5 false] [(in-ctx a "(q/m nil)") (in-ctx b "(satisfies? q/P nil)")]))))
  (testing "the context that owns the MultiFn writes it in place: a held value sees the method"
    (let [base (sci/init {})]
      (is (= [1 true] (in-ctx base "(defmulti mm identity) (def held mm) (defmethod mm :a [_] 1) [(held :a) (identical? held mm)]")))
      (let [a (sci/fork base)]
        (is (= [2 3 true] (in-ctx a "(defmethod mm :a [_] 2) (def held2 mm) (defmethod mm :b [_] 3) [(held2 :a) (held2 :b) (identical? held2 mm)]")))
        (is (= 1 (in-ctx base "(mm :a)")))))))
