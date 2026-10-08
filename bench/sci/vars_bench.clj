(ns sci.vars-bench
  "Read- and write-heavy Var benchmarks: deref and call through a Var in a
  hot interpreted loop, in an unforked context, a fork that inherits the Var
  and a fork that redefined it (x and f; ux and uf are never redefined); fork creation over 5,700 Vars; a def in a fork.
  Run: clojure -M:test -i bench/sci/vars_bench.clj -m sci.vars-bench
  (the first run warms the JIT; the second is printed)"
  (:refer-clojure :exclude [bean])
  (:require [clojure.pprint :as pprint]
            [sci.core :as sci]
            [sci.ctx-store]))

(set! *warn-on-reflection* true)

(def ^com.sun.management.ThreadMXBean bean
  (java.lang.management.ManagementFactory/getThreadMXBean))

(defn- alloc [] (.getThreadAllocatedBytes bean (.getId (Thread/currentThread))))

(defn- sample
  "ns and allocated bytes per op of n calls of f, median of 5 samples."
  [f n]
  (let [one (fn []
              (let [a (alloc) t (System/nanoTime)]
                (f)
                [(/ (double (- (System/nanoTime) t)) n)
                 (/ (double (- (alloc) a)) n)]))
        _ (dotimes [_ 3] (one))
        xs (sort-by first (repeatedly 5 one))
        [ns b] (nth xs 2)]
    {:ns-per-op (/ (Math/round (* 100.0 (double ns))) 100.0)
     :bytes-per-op (Math/round (double b))}))

(def loop-n 1000000)

(def prog
  "(def x 1) (defn f [a] a)
   (defn read-loop [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc x)) acc)))
   (defn call-loop [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc (f 1))) acc)))
   (defn empty-loop [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc 1)) acc)))
   (def ux 1) (defn uf [a] a)
   (defn untouched-read-loop [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc ux)) acc)))
   (defn untouched-call-loop [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc (uf 1))) acc)))")

(defn- loops [ctx]
  (let [r (sci/eval-string* ctx "read-loop")
        c (sci/eval-string* ctx "call-loop")
        e (sci/eval-string* ctx "empty-loop")
        ur (sci/eval-string* ctx "untouched-read-loop")
        uc (sci/eval-string* ctx "untouched-call-loop")
        run (fn [g] (fn [] (sci/binding [] (sci.ctx-store/with-ctx ctx (g loop-n)))))]
    {:empty-loop (sample (run e) loop-n)
     :read-loop (sample (run r) loop-n)
     :call-loop (sample (run c) loop-n)
     :untouched-read-loop (sample (run ur) loop-n)
     :untouched-call-loop (sample (run uc) loop-n)}))

(defn- big-ctx []
  (let [ctx (sci/init {})]
    (sci/eval-string* ctx (apply str "(ns bench)"
                                 (map #(str "(defn f" % " [a] a)") (range 5700))))
    (sci/eval-string* ctx "(ns user)")
    ctx))

(defn run []
  (let [base (sci/init {})
        _ (sci/eval-string* base prog)
        inherited (sci/fork base)
        redefined (sci/fork base)
        _ (sci/eval-string* redefined "(def x 1) (defn f [a] a)")
        big (big-ctx)
        held (sci/resolve base 'x)
        n 1000000]
    {:unforked (loops base)
     :fork-inherits (loops inherited)
     :fork-redefined (loops redefined)
     :host-deref-held-var (sample #(dotimes [_ n] (.deref ^clojure.lang.IDeref held)) n)
     :fork-5700-vars (sample #(dotimes [_ 1000] (sci/fork big)) 1000)
     :def-new-in-fork (let [c (sci/fork base)
                            node "(def y 2)"]
                        (sample #(dotimes [_ 200] (sci/eval-string* (sci/fork c) node)) 200))
     :redef-inherited-in-fork (sample #(dotimes [_ 200] (sci/eval-string* (sci/fork base) "(def x 2)")) 200)
     :redef-then-call (sample #(dotimes [_ 200]
                                 (sci/eval-string* (sci/fork base) "(defn f [a] (inc a)) (call-loop 10)"))
                              200)}))

(defn -main [& _]
  (run)
  (let [r (run)]
    (pprint/pprint r)
    (shutdown-agents)))
