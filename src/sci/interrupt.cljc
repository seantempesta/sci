(ns sci.interrupt
  "Opt-in, interrupt-fn aware overrides for core functions that iterate over
  host sequences without ever entering an interpreted function, and therefore
  never hit the per-fn-entry `:interrupt-fn` check.

  This namespace is NOT loaded by default and adds nothing to a standard SCI
  build unless you require it. Merge `clojure-core` into your context's
  `clojure.core` namespace to make these functions interruptible:

      (require '[sci.core :as sci]
               '[sci.interrupt :as interrupt])

      (sci/init {:interrupt-fn my-interrupt-fn
                 :namespaces {'clojure.core interrupt/clojure-core}})

  Each override reads `:interrupt-fn` from the current context at call time and
  falls back to the native function when no `:interrupt-fn` is configured, so
  merging `clojure-core` is always safe."
  (:require
   [clojure.string :as str]
   [sci.ctx-store :as store]
   [sci.impl.copy-vars :as copy-vars]
   [sci.impl.utils :as utils]))

(defn get-interrupt-fn
  "Returns the `:interrupt-fn` configured on `ctx` (as produced by `sci/init`),
  or nil when none was set. Use this from host functions you expose to make them
  interrupt-fn aware, e.g. `(get-interrupt-fn (sci.ctx-store/get-ctx))`."
  [ctx]
  (:interrupt-fn ctx))

(defn interrupt!
  "Throws an interrupt signal that sandboxed code cannot catch. Call this from
  your `:interrupt-fn` instead of throwing a plain exception, so evaluated code
  cannot swallow the interrupt with `try`/`catch` and keep running. The thrown
  value is an `ex-info` carrying a private marker; `sci`'s `try` refuses to hand
  it to user `catch` clauses, and sandboxed code cannot forge it. It propagates
  to the host calling `eval-string`."
  ([] (interrupt! "Interrupted"))
  ([msg] (interrupt! msg {}))
  ([msg data]
   (throw (ex-info msg (assoc data :sci.impl/interrupt utils/interrupt-marker)))))

;;; Producers - lazy sequences that fire interrupt-fn on each step

(defn- range-seq
  "Wraps the core range `s` so `ifn` fires once per chunk (once per element for
  an unchunked seq). Chunks come from core `range`, so element production costs
  what core costs, and `count`/`reduce` walk whole chunks."
  [s ifn]
  (lazy-seq
    (when-let [s (seq s)]
      (ifn)
      (if (chunked-seq? s)
        (chunk-cons (chunk-first s) (range-seq (chunk-rest s) ifn))
        (cons (first s) (range-seq (rest s) ifn))))))

;; A finite long range keeps core `LongRange`'s O(1) `count` and chunked
;; `reduce`: this seq delegates to one and fires `ifn` per chunk.
#?(:clj
   (declare ->CheckedRange))

#?(:clj
   (defn- reduce-chunks [^clojure.lang.LongRange r ^clojure.lang.IFn ifn ^clojure.lang.IFn f init]
     (loop [s r acc init]
       (ifn)
       (let [acc (.reduce (.chunkedFirst s) f acc)]
         (if (reduced? acc)
           @acc
           (if-let [n (.chunkedNext s)] (recur n acc) acc))))))

#?(:clj
   (deftype CheckedRange [^clojure.lang.LongRange r ^clojure.lang.IFn ifn m]
     clojure.lang.Sequential
     clojure.lang.ISeq
     (first [_] (.first r))
     (next [_] (when-let [n (.next r)] (ifn) (CheckedRange. n ifn nil)))
     (more [this] (or (.next this) ()))
     (cons [_ o] (.cons r o))
     clojure.lang.Counted
     (count [_] (.count r))
     clojure.lang.IPersistentCollection
     (empty [_] ())
     (equiv [_ o] (.equiv r (if (instance? CheckedRange o) (.-r ^CheckedRange o) o)))
     clojure.lang.Seqable
     (seq [this] this)
     clojure.lang.IChunkedSeq
     (chunkedFirst [_] (ifn) (.chunkedFirst r))
     (chunkedNext [_] (when-let [n (.chunkedNext r)] (CheckedRange. n ifn nil)))
     (chunkedMore [this] (or (.chunkedNext this) ()))
     clojure.lang.IReduceInit
     (reduce [_ f init] (reduce-chunks r ifn f init))
     clojure.lang.IReduce
     (reduce [_ f]
       (let [a (.first r)]
         (if-let [n (.next r)] (reduce-chunks n ifn f a) a)))
     clojure.lang.IHashEq
     (hasheq [_] (.hasheq r))
     clojure.lang.IObj
     (withMeta [_ m'] (CheckedRange. r ifn m'))
     clojure.lang.IMeta
     (meta [_] m)
     Iterable
     (iterator [this] (clojure.lang.SeqIterator. (range-seq r ifn)))))

(defn- checked-range? [coll]
  #?(:clj (instance? CheckedRange coll) :cljs false))

(defn- checked-range [s ifn]
  #?(:clj (if (instance? clojure.lang.LongRange s) (CheckedRange. s ifn nil) (range-seq s ifn))
     :cljs (range-seq s ifn)))

(defn- sci-range
  ([]    (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (range-seq (range) ifn) (range))))
  ([end] (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (checked-range (range end) ifn) (range end))))
  ([start end]
   (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (checked-range (range start end) ifn) (range start end))))
  ([start end step]
   (let [ifn (get-interrupt-fn (store/get-ctx))] (if ifn (checked-range (range start end step) ifn) (range start end step)))))

(defn- sci-repeat
  ([x]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (repeat x)
       (letfn [(gen [] (lazy-seq (ifn) (cons x (gen))))]
         (gen)))))
  ([n x]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (repeat n x)
       (letfn [(gen [i]
                 (lazy-seq
                   (when (pos? i)
                     (ifn)
                     (cons x (gen (dec i))))))]
         (gen n))))))

(defn- sci-cycle [coll]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if (or (not ifn) (not (seq coll)))
      (cycle coll)
      (letfn [(gen [s]
                (lazy-seq
                  (ifn)
                  (let [cur (or (seq s) (seq coll))]
                    (cons (first cur) (gen (rest cur))))))]
        (gen coll)))))

(defn- sci-iterate [f x]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if-not ifn
      (iterate f x)
      (letfn [(gen [v]
                (lazy-seq
                  (ifn)
                  (cons v (gen (f v)))))]
        (gen x)))))

;;; Regex - fire interrupt-fn during backtracking
;;;
;;; Catastrophic backtracking (ReDoS) happens inside a single host `.matches`/
;;; `.find` call, which never enters an interpreted fn, so the per-fn-entry
;;; `:interrupt-fn` check never runs. Wrapping the input in a `CharSequence`
;;; whose `charAt` fires `ifn` makes the match abortable: backtracking re-reads
;;; characters, so `ifn` is called often.
;;;
;;; CLJS regex is the JS engine: no char-access hook to fire `ifn` on, and JS is
;;; single-threaded so a running match blocks the event loop and can't be
;;; interrupted in-thread. The technique is JVM-only, so these overrides are not
;;; defined in CLJS and are absent from `clojure-core` there.

#?(:clj
   (deftype ^:private InterruptibleCS [^CharSequence s ifn]
     CharSequence
     (length [_] (.length s))
     (charAt [_ i] (ifn) (.charAt s i))
     (subSequence [_ a b] (.subSequence s a b))
     (toString [_] (.toString s))))

#?(:clj
   (defn- sci-re-matcher [re s]
     (let [ifn (get-interrupt-fn (store/get-ctx))]
       (if-not ifn
         (re-matcher re s)
         (re-matcher re (InterruptibleCS. s ifn))))))

#?(:clj
   (defn- sci-re-matches [re s]
     (let [ifn (get-interrupt-fn (store/get-ctx))]
       (if-not ifn
         (re-matches re s)
         (let [m (re-matcher re (InterruptibleCS. s ifn))]
           (when (.matches m) (re-groups m)))))))

#?(:clj
   (defn- sci-re-find
     ([m] (re-find m))
     ([re s]
      (let [ifn (get-interrupt-fn (store/get-ctx))]
        (if-not ifn
          (re-find re s)
          (let [m (re-matcher re (InterruptibleCS. s ifn))]
            (when (.find m) (re-groups m))))))))

#?(:clj
   (defn- sci-re-seq [re s]
     (let [ifn (get-interrupt-fn (store/get-ctx))]
       (if-not ifn
         (re-seq re s)
         (let [m (re-matcher re (InterruptibleCS. s ifn))]
           ((fn step []
              (when (.find m)
                (lazy-seq (cons (re-groups m) (step)))))))))))

#?(:clj
   (def ^:private replace-by #'str/replace-by))

#?(:clj
   (defn- sci-string-replace [^CharSequence s match replacement] 
     (if (instance? java.util.regex.Pattern match)
       (let [ifn (get-interrupt-fn (store/get-ctx))]
         (if-not ifn
           (str/replace s match replacement)
           (if (instance? CharSequence replacement)
              (.replaceAll (re-matcher ^java.util.regex.Pattern match (InterruptibleCS. s ifn))
                             (.toString ^CharSequence replacement))
              (replace-by (InterruptibleCS. s ifn) match replacement))))
       (str/replace s match replacement))))

#?(:clj
   (def ^:private replace-first-by #'str/replace-first-by))

#?(:clj
   (defn- sci-string-replace-first [^CharSequence s match replacement] 
     (if (instance? java.util.regex.Pattern match)
       (let [ifn (get-interrupt-fn (store/get-ctx))]
         (if-not ifn
           (str/replace s match replacement)
           (if (instance? CharSequence replacement)
              (.replaceFirst (re-matcher ^java.util.regex.Pattern match (InterruptibleCS. s ifn))
                             (.toString ^CharSequence replacement))
              (replace-first-by (InterruptibleCS. s ifn) match replacement))))
       (str/replace s match replacement))))

#?(:clj
   (defn- sci-string-split
     ([s re]      
      (let [ifn (get-interrupt-fn (store/get-ctx))]
        (if-not ifn
          (str/split s re)
          (str/split (InterruptibleCS. s ifn) re))))
     ([s re limit]
      (let [ifn (get-interrupt-fn (store/get-ctx))]
        (if-not ifn
          (str/split s re limit)
          (str/split (InterruptibleCS. s ifn) re limit))))))

;;; Materializers - consuming functions that fire interrupt-fn per element

(defn- sci-dorun
  ([coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (dorun coll)
       (loop [s (seq coll)]
         (when s
           (ifn)
           (recur (next s)))))))
  ([n coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (dorun n coll)
       (loop [s (seq coll) i 0]
         (when (and s (< i n))
           (ifn)
           (recur (next s) (inc i))))))))

(defn- sci-doall
  ([coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (doall coll)
       (do (loop [s (seq coll)]
             (when s (ifn) (recur (next s))))
           coll))))
  ([n coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (doall n coll)
       (do (loop [s (seq coll) i 0]
             (when (and s (< i n)) (ifn) (recur (next s) (inc i))))
           coll)))))

(defn- sci-count [coll]
  (let [ifn (get-interrupt-fn (store/get-ctx))]
    (if (or (not ifn) (counted? coll))
      (count coll)
      (loop [s (seq coll) n 0]
        (if s
          (do (ifn) (recur (next s) (inc n)))
          n)))))

(defn- sci-into
  ([] (into))
  ([to] (into to))
  ([to from]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (cond
       (or (not ifn) (checked-range? from)) (into to from)
       #?@(:clj [(instance? clojure.lang.IEditableCollection to)
                 (with-meta (persistent! (reduce (fn [acc x] (ifn) (conj! acc x)) (transient to) (seq from))) (meta to))])
       :else (reduce (fn [acc x] (ifn) (conj acc x)) to from))))
  ([to xf from]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (into to xf from)
       (transduce (comp (map (fn [x] (ifn) x)) xf) conj to from)))))

(defn- sci-reduce
  ([f coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))
         s   (seq coll)]
     (if-not ifn
       (reduce f coll)
       (if (checked-range? coll)
         (reduce f coll)
       (if s
         (loop [v (first s) s (next s)]
           (if s
             (do (ifn)
                 (let [ret (f v (first s))]
                   (if (reduced? ret) @ret (recur ret (next s)))))
             v))
         (f))))))
  ([f init coll]
   (let [ifn (get-interrupt-fn (store/get-ctx))]
     (if-not ifn
       (reduce f init coll)
       (if (checked-range? coll)
         (reduce f init coll)
       (loop [v init s (seq coll)]
         (if s
           (do (ifn)
               (let [ret (f v (first s))]
                 (if (reduced? ret) @ret (recur ret (next s)))))
           v)))))))

(def clojure-core
  "Map of `clojure.core` symbol -> interrupt-fn aware replacement var. Each value
  is a real SCI var on the shared `clojure.core` namespace object (built via
  `copy-vars/new-var`, like the built-in core vars), so it carries proper
  `:ns`/`:name`/`:sci/built-in` metadata. Merge into `{:namespaces {'clojure.core ...}}`."
  {'range   (copy-vars/new-var 'range   sci-range   true)
   'repeat  (copy-vars/new-var 'repeat  sci-repeat  true)
   'cycle   (copy-vars/new-var 'cycle   sci-cycle   true)
   'iterate (copy-vars/new-var 'iterate sci-iterate true)
   'doall   (copy-vars/new-var 'doall   sci-doall   true)
   'dorun   (copy-vars/new-var 'dorun   sci-dorun   true)
   'count   (copy-vars/new-var 'count   sci-count   true)
   'into    (copy-vars/new-var 'into    sci-into    true)
   'reduce  (copy-vars/new-var 'reduce  sci-reduce  true)
   #?@(:clj ['re-find    (copy-vars/new-var 're-find    sci-re-find    true)
             're-matcher (copy-vars/new-var 're-matcher sci-re-matcher true)
             're-matches (copy-vars/new-var 're-matches sci-re-matches true)
             're-seq     (copy-vars/new-var 're-seq     sci-re-seq     true)])})

(def clojure-string
  "Map of `clojure.string` symbol -> interrupt-fn aware replacement var. Each value
  is a real SCI var on the shared `clojure.string` namespace object (built via
  `copy-vars/new-var`, like the built-in core vars), so it carries proper
  `:ns`/`:name`/`:sci/built-in` metadata. Merge into `{:namespaces {'clojure.string ...}}`."
  {#?@(:clj ['replace       (copy-vars/new-var 'replace sci-string-replace       true)
             'replace-first (copy-vars/new-var 'replace sci-string-replace-first true)
             'split         (copy-vars/new-var 'split   sci-string-split         true)])})
