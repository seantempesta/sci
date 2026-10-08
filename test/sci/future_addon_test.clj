(ns sci.future-addon-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.addons.future :as future]
            [sci.core :as sci]))

(deftest injected-future-call-test
  (let [calls (atom 0)
        counting (fn [thunk] (swap! calls inc) (future-call thunk))
        opts (future/install {} {:future-call counting})]
    (is (= '(2 3 4) (sci/eval-string "(doall (pmap inc [1 2 3]))" opts)))
    (is (= 3 @calls))
    (is (= 1 (sci/eval-string "(deref (future 1))" opts)))
    (is (= 4 @calls))))

(deftest default-future-call-test
  (let [opts (future/install {})]
    (is (= '(2 3 4) (sci/eval-string "(doall (pmap inc [1 2 3]))" opts)))
    (is (= 1 (sci/eval-string "(deref (future 1))" opts)))
    (is (= 3 (sci/eval-string "(deref (future-call (fn [] 3)))" opts)))))
