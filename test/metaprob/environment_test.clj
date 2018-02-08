(ns metaprob.environment-test
  (:require [clojure.test :refer :all]
            [metaprob.environment :refer :all]))

(deftest predications
  (testing "Check the environment? predicate"
    (let [env (make-top-level-env *ns*)]
      (is (environment? env))
      (is (environment? (make-sub-environment env)))
      (is (environment? (make-sub-environment (make-sub-environment env)))))))

(deftest top-level
  (testing "Fetch from top level environment"
    (let [env (make-top-level-env *ns*)]
      (is (= (env-lookup env 'list)
             list)))))

(deftest frame-in-out
  (testing "Store then fetch to/from frame"
    (is (= (dosync
            (let [top (make-top-level-env *ns*)
                  env (make-sub-environment top)]
              (env-bind! env "foo" 7)
              (env-lookup env "foo")))
           7))))
