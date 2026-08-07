(ns sci.call-preparation-hook-test
  "JVM only, deliberately.

  The hook fires where `return-call` resolves the callee to a Var. On
  ClojureScript, calls to a Var go through the var-deref path instead, so
  neither this hook nor `:built-in-call-observer` sees them: the existing
  `sci.interop-test/built-in-call-observer-test` already fails under
  `script/test/node` at the pin for that reason. When the CLJS call path
  grows Var-aware call nodes, this namespace becomes `.cljc`."
  (:require
   [clojure.test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.impl.analyzer :as analyzer]
   [sci.impl.types :as types]
   [sci.impl.vars :as vars]))

(defn- read-rows
  "A host function whose first parameter is the one a hook supplies."
  [db query]
  {:read/from db :read/query query})

(defn- annotate
  "A host function no hook in this namespace claims."
  [x]
  {:annotated x})

(defn- hook-ctx
  "Context with `read-rows`/`annotate` installed in `my`, and `hook` installed."
  [hook]
  (let [my-ns (sci/create-ns 'my)]
    (sci/init {:namespaces {'my {'read-rows (sci/copy-var read-rows my-ns)
                                 'annotate (sci/copy-var annotate my-ns)}}
               :call-preparation-hook hook})))

(defn- supply-db
  "A hook supplying `db` as the first argument of `my/read-rows` only."
  [db]
  (fn [_ctx v args]
    (if (and (= 'my/read-rows (vars/toSymbol v))
             (= 1 (count args)))
      (into [db] args)
      args)))

(deftest prepares-arguments-test
  (let [ctx (hook-ctx (supply-db :db-alpha))]
    (testing "an argument the caller omitted is supplied by the hook"
      (is (= {:read/from :db-alpha :read/query :q}
             (sci/eval-string* ctx "(my/read-rows :q)"))))
    (testing "the caller's own arguments win"
      (is (= {:read/from :db-explicit :read/query :q}
             (sci/eval-string* ctx "(my/read-rows :db-explicit :q)"))))
    (testing "a call site the hook does not claim is untouched"
      (is (= {:annotated 1} (sci/eval-string* ctx "(my/annotate 1)"))))
    (testing "nested and interpreted call sites are prepared alike"
      (is (= [{:read/from :db-alpha :read/query :q}]
             (sci/eval-string* ctx "(vector (my/read-rows :q))")))
      (is (= {:read/from :db-alpha :read/query :q}
             (sci/eval-string* ctx "(do (defn f [q] (my/read-rows q)) (f :q))"))))))

(deftest reduced-result-short-circuits-test
  (let [entered (atom false)
        my-ns (sci/create-ns 'my)
        ctx (sci/init {:namespaces
                       {'my {'read-rows (sci/new-var 'read-rows
                                                     (fn [& _]
                                                       (reset! entered true)
                                                       :entered)
                                                     {:ns my-ns})}}
                       :call-preparation-hook
                       (fn [_ctx _v _args] (reduced {:error :unavailable}))})]
    (is (= {:error :unavailable} (sci/eval-string* ctx "(my/read-rows :db :q)")))
    (is (false? @entered) "a reduced result is returned without entering the callee")))

(deftest hook-is-read-from-the-executing-context-test
  (testing "one analyzed node, two forks, two hooks"
    (let [base (hook-ctx nil)
          node (analyzer/analyze base '(my/read-rows :q))
          alpha (assoc (sci/fork base) :call-preparation-hook (supply-db :db-alpha))
          beta (assoc (sci/fork base) :call-preparation-hook (supply-db :db-beta))]
      (is (= :db-alpha (:read/from (types/eval node alpha nil))))
      (is (= :db-beta (:read/from (types/eval node beta nil))))
      (is (= :db-alpha (:read/from (types/eval node alpha nil)))
          "no re-analysis happens between the two executions")
      (is (thrown? Exception (types/eval node base nil))
          "a context with no hook still sees the callee's real arity"))))

(deftest unhooked-call-shapes-test
  (let [ctx (hook-ctx (fn [_ctx _v _args] (reduced :hooked)))]
    (testing "a computed callee is not a Var and is not hooked"
      (is (= 3 (sci/eval-string* ctx "(((fn [] +)) 1 2)"))))
    (testing "a local fn is not a Var and is not hooked"
      (is (= 3 (sci/eval-string* ctx "(let [f +] (f 1 2))"))))))

(deftest concurrent-forks-test
  (testing "each thread's call resolves the fork it is executing under"
    (let [base (hook-ctx nil)
          forks (mapv (fn [i]
                        (assoc (sci/fork base)
                               :call-preparation-hook
                               (supply-db (keyword (str "db-" i)))))
                      (range 8))
          results (->> (for [i (range 8), _ (range 40)]
                         [i (future (:read/from
                                     (sci/eval-string* (nth forks i)
                                                       "(my/read-rows :q)")))])
                       (mapv (fn [[i fut]] [(keyword (str "db-" i)) @fut])))]
      (is (= 320 (count results)))
      (is (empty? (remove (fn [[expected actual]] (= expected actual)) results))))))
