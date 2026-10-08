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
    (is (= :base (h)) "with no context, the Var's own binding")))

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
    (testing "a base fn passed by value runs under the base (its defining context)"
      (is (= [1 :first] (ev fork "(host/thread calls-priv)"))))))
