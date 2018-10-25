(ns metaprob.state-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [metaprob.state :as state])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [metaprob.state :as state])))

(deftest keys-1
  (testing "state-keys smoke tests"
    (is (= (state/state-keys '()) '()))
    (is (= (state/state-keys '(a)) (list state/rest-marker)))
    (is (= (state/state-keys '[17]) (list 0)))
    (is (= (state/state-keys '{"a" 17}) (list "a")))
    (is (= (state/state-keys '{"a" 17 :value 19}) (list "a")))))

(deftest state-to-from-map-1
  (testing "tests for state-to-map"
    (let [a [7 8]
          b {0 {:value 7} 1 {:value 8}}]
      (is (= (state/state-to-map a) b))
      (is (= (state/map-to-state b) a)))))

(deftest has-value?-1
  (testing "tests for has-value?"
    (is (not (state/has-value? '{})))
    (is      (state/has-value? '{:value 7}))
    (is      (state/has-value? '{:value nil}))
    (is (not (state/has-value? '())))
    (is      (state/has-value? '(7 8)))
    (is (not (state/has-value? '[])))
    (is (not (state/has-value? '[7 8])))))

(deftest value-1
  (testing "tests for value"
    (is (= 7   (state/value '{:value 7})))
    (is (= nil (state/value '{:value nil})))
    (is (= 7   (state/value '(7 8))))))

(deftest has-subtrace?-1
  (testing "tests for has-subtrace?"
    (is (not (state/has-subtrace? '{} "foo")))
    (is      (state/has-subtrace? '{"foo" 7} "foo"))
    (is (not (state/has-subtrace? '{"foo" 7} "bar")))
    (is      (state/has-subtrace? '{"foo" nil} "foo"))
    (is (not (state/has-subtrace? '() "foo")))
    (is (not (state/has-subtrace? '() state/rest-marker)))
    (is      (state/has-subtrace? '(7 8) state/rest-marker))
    (is (not (state/has-subtrace? '(7 8) 7)))
    (is (not (state/has-subtrace? '[] 0)))
    (is (not (state/has-subtrace? '[] 1)))
    (is      (state/has-subtrace? '[7 8] 1))
    (is (not (state/has-subtrace? '[7 8] 3)))))

(deftest subtrace-1
  (testing "tests for subtrace"
    (is (= (state/subtrace '{"foo" {"bar" 7}} "foo") {"bar" 7}))
    (is (= (state/subtrace '(7 8 9) state/rest-marker) '(8 9)))
    (is (= (state/subtrace [7 {"foo" 8} 9] 1) {:value {"foo" 8}}))))

(deftest subtrace-count-1
  (testing "tests for subtrace-count"
    (is (= (state/subtrace-count '{"foo" {"bar" 7} "quux" {"baz" 8}}) 2))
    (is (= (state/subtrace-count '{"foo" {"bar" 7} :value {"baz" 8}}) 1))
    (is (= (state/subtrace-count '(7 8 9)) 1))
    (is (= (state/subtrace-count [7 {"foo" 8} 9]) 3))))

(deftest set-value-1
  (testing "tests for set-value"
    (is (= (state/value (state/set-value {} 17)) 17))
    (is (= (state/value (state/set-value {"foo" 7} 17)) 17))
    (is (= (state/value (state/set-value '() 17)) 17))
    (is (= (state/value (state/set-value '(18 19) 17)) 17))
    (is (= (state/value (state/set-value [] 17)) 17))
    (is (= (state/value (state/set-value [18 19] 17)) 17))))

(deftest set-subtrace-1
  (testing "tests for set-subtrace"
    (is (vector? (state/set-subtrace {} 0 {:value 17})))))
