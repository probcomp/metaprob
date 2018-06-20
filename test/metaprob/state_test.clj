(ns metaprob.state-test
  (:require [clojure.test :refer :all]
            [metaprob.state :refer :all]))

(deftest keys-1
  (testing "state-keys smoke tests"
    (is (= (state-keys '()) '()))
    (is (= (state-keys '(a)) (list rest-marker)))
    (is (= (state-keys '[17]) (list 0)))
    (is (= (state-keys '{"a" 17}) (list "a")))
    (is (= (state-keys '{"a" 17 :value 19}) (list "a")))))

(deftest state-to-from-map-1
  (testing "tests for state-to-map"
    (let [a [7 8]
          b {0 {:value 7} 1 {:value 8}}]
      (is (= (state-to-map a) b))
      (is (= (map-to-state b) a)))))

(deftest has-value?-1
  (testing "tests for has-value?"
    (is (not (has-value? '{})))
    (is      (has-value? '{:value 7}))
    (is      (has-value? '{:value nil}))
    (is (not (has-value? '())))
    (is      (has-value? '(7 8)))
    (is (not (has-value? '[])))
    (is (not (has-value? '[7 8])))))

(deftest value-1
  (testing "tests for value"
    (is (= (value '{:value 7}) 7))
    (is (= (value '{:value nil}) nil))
    (is (= (value '(7 8)) 7))))

(deftest has-subtrace?-1
  (testing "tests for has-subtrace?"
    (is (not (has-subtrace? '{} "foo")))
    (is      (has-subtrace? '{"foo" 7} "foo"))
    (is (not (has-subtrace? '{"foo" 7} "bar")))
    (is      (has-subtrace? '{"foo" nil} "foo"))
    (is (not (has-subtrace? '() "foo")))
    (is (not (has-subtrace? '() rest-marker)))
    (is      (has-subtrace? '(7 8) rest-marker))
    (is (not (has-subtrace? '(7 8) 7)))
    (is (not (has-subtrace? '[] 0)))
    (is (not (has-subtrace? '[] 1)))
    (is      (has-subtrace? '[7 8] 1))
    (is (not (has-subtrace? '[7 8] 3)))))

(deftest subtrace-1
  (testing "tests for subtrace"
    (is (= (subtrace '{"foo" {"bar" 7}} "foo") {"bar" 7}))
    (is (= (subtrace '(7 8 9) rest-marker) '(8 9)))
    (is (= (subtrace [7 {"foo" 8} 9] 1) {:value {"foo" 8}}))))

(deftest subtrace-count-1
  (testing "tests for subtrace-count"
    (is (= (subtrace-count '{"foo" {"bar" 7} "quux" {"baz" 8}}) 2))
    (is (= (subtrace-count '{"foo" {"bar" 7} :value {"baz" 8}}) 1))
    (is (= (subtrace-count '(7 8 9)) 1))
    (is (= (subtrace-count [7 {"foo" 8} 9]) 3))))

(deftest set-value-1
  (testing "tests for set-value"
    (is (= (value (set-value {} 17)) 17))
    (is (= (value (set-value {"foo" 7} 17)) 17))
    (is (= (value (set-value '() 17)) 17))
    (is (= (value (set-value '(18 19) 17)) 17))
    (is (= (value (set-value [] 17)) 17))
    (is (= (value (set-value [18 19] 17)) 17))))

(deftest set-subtrace-1
  (testing "tests for set-subtrace"
    (is (vector? (set-subtrace {} 0 {:value 17})))))

