(ns sci.addons.future
  {:no-doc true}
  (:refer-clojure :exclude [pmap])
  (:require [sci.impl.copy-vars :refer [copy-core-var new-var macrofy]])
  (:require [sci.impl.vars :as vars]))

(def future* (macrofy 'future
               (fn [_ _ & body]
                 `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
                    (~'future-call f#)))))

(defmacro future**
  "Like clojure.core/future but also conveys sci bindings to the thread."
  [& body]
  `(let [f# (-> (fn [] ~@body)
                (vars/binding-conveyor-fn))]
     (future-call f#)))

(defn- pmap-with
  "pmap whose elements run through `future-call` (a host fn of one thunk)."
  [future-call]
  (fn pmap
    ([f coll]
     (let [n (+ 2 (.. Runtime getRuntime availableProcessors))
           rets (map #(future-call (vars/binding-conveyor-fn (fn [] (f %)))) coll)
           step (fn step [[x & xs :as vs] fs]
                  (lazy-seq
                   (if-let [s (seq fs)]
                     (cons (deref x) (step xs (rest s)))
                     (map deref vs))))]
       (step rets (drop n rets))))
    ([f coll & colls]
     (let [step (fn step [cs]
                  (lazy-seq
                   (let [ss (map seq cs)]
                     (when (every? identity ss)
                       (cons (map first ss) (step (map rest ss)))))))]
       (pmap #(apply f %) (step (cons coll colls)))))))

(def pmap
  "Like clojure.core/pmap but also conveys sci bindings to the threads."
  (pmap-with future-call))

(defn install
  "Installs the future functions; `future-call` (a host fn of one thunk) replaces
  the host `future-call` for `future-call`, `future` and `pmap`."
  ([opts] (install opts {}))
  ([opts {injected-call :future-call :as injected}]
   (update-in opts [:namespaces 'clojure.core]
              assoc
              'future future*
              'future-call (if (contains? injected :future-call)
                             (new-var 'future-call injected-call)
                             (copy-core-var future-call))
              'future-cancel (copy-core-var future-cancel)
              'future-cancelled? (copy-core-var future-cancelled?)
              'future-done? (copy-core-var future-done?)
              'future? (copy-core-var future?)
              'pmap (new-var 'pmap (if (contains? injected :future-call)
                                     (pmap-with injected-call)
                                     pmap)))))
