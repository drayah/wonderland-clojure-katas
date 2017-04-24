(ns wonderland-number.finder-test
  (:require [clojure.test :refer :all]
            [wonderland-number.finder :refer :all]))

(defn hasAllTheSameDigits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(deftest test-candidates
  (testing "Should have a range of candidates"
    (is (= 100000 (first candidates)))
    (is (= 999999 (last candidates)))))

(deftest test-same
  (testing "Numbers with same digits should be the same"
    (is (true? (same 12345 52431 12354 12354 12354 12354 12354)))
    (is (true? (same 99999 99999)))
    (is (true? (same 767 776 677)))
    (is (false? (same 123 124)))
    (is (true? (same 1)))
    (is (true? (same 1 1 1)))
    (is (false? (same 1 1 1 4)))
    (is (false? (same 333 444)))))

(deftest test-wonderland-number
  (testing "A wonderland number must have the following things true about it"
    (let [wondernum (wonderland-number)]
      (is (= 6 (count (str wondernum))))
      (is (hasAllTheSameDigits? wondernum (* 2 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 3 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 4 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 5 wondernum)))
      (is (hasAllTheSameDigits? wondernum (* 6 wondernum))))))