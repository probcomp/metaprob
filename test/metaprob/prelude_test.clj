(ns metaprob.prelude-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :as builtin])
  (:require [metaprob.prelude :as prelude]))

;;  (:refer metaprob.builtin
;;          :exclude [map reverse zipmap iterate concat drop replicate filter repeat])
;;  (:refer metaprob.prelude
;;          :exclude [not assert pprint and or
;;                    list first rest last nth range])

(deftest smoke-1
  (testing "Prelude smoke test"
    (is (builtin/trace_get
         (builtin/lookup (prelude/sp "foo" (program [] "foo"))
                         (builtin/list "custom_interpreter"))))
    ;; These tests have to run after the call to sp
    (is (= (ns-resolve 'metaprob.prelude 'v) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'v))
        "namespacing sanity check 2")))

(deftest map-1
  (testing "map smoke test"
    (is (builtin/nth (prelude/map (program [x] (builtin/add x 1))
                                  (builtin/list 4 5 6))
                     1)
        6)
    ;; These tests have to run after the call to map
    (is (= (ns-resolve 'metaprob.prelude 'val) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'val))
        "namespacing sanity check 2")))

;; I'm sort of tired of this and don't anticipate problems, so
;; not putting more work into tests at this time.
