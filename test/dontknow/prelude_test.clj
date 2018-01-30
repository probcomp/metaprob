(ns dontknow.prelude-test
  (:require [clojure.test :refer :all]
            [dontknow.trie :refer :all]
            [dontknow.syntax :refer :all])
  (:require [dontknow.builtin :as builtin])
  (:require [dontknow.prelude :as prelude]))

;;  (:refer dontknow.builtin
;;          :exclude [map reverse zipmap iterate concat drop replicate filter repeat])
;;  (:refer dontknow.prelude
;;          :exclude [not assert pprint and or
;;                    list first rest last nth range])

(deftest smoke-1
  (testing "Prelude smoke test"
    (is (builtin/trace_get
         (builtin/lookup (prelude/sp "foo" (program [] "foo"))
                         (builtin/list "custom_interpreter"))))
    ;; These tests have to run after the call to sp
    (is (= (ns-resolve 'dontknow.prelude 'v) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'dontknow.prelude) 'v))
        "namespacing sanity check 2")))

(deftest map-1
  (testing "map smoke test"
    (is (builtin/nth (prelude/map (program [x] (builtin/add x 1))
                                  (builtin/list 4 5 6))
                     1)
        6)
    ;; These tests have to run after the call to map
    (is (= (ns-resolve 'dontknow.prelude 'val) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'dontknow.prelude) 'val))
        "namespacing sanity check 2")))

;; I'm sort of tired of this and don't anticipate problems, so
;; not putting more work into tests at this time.
