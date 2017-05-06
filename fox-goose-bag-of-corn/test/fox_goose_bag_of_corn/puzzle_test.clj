(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer :all]
            [fox-goose-bag-of-corn.puzzle :refer :all]
            [clojure.set]))

(defn validate-move [step1 step2]
  (testing "only you and another thing can move"
    (let [diff1 (clojure.set/difference step1 step2)
          diff2 (clojure.set/difference step2 step1)
          diffs (concat diff1 diff2)
          diff-num (count diffs)]
      (is (> 3 diff-num))
      (when (pos? diff-num)
        (is (contains? (set diffs) :you)))
      step2)))

(deftest test-invalid-left-right?
  (testing "should return true when fox alone with goose"
    (is (true? (invalid-left-right? [:fox :goose]))))
  (testing "should return true when goose alone with corn"
    (is (true? (invalid-left-right? [:goose :corn]))))
  (testing "should return true when fox, goose and corn alone"
    (is (true? (invalid-left-right? [:goose :fox :corn]))))
  (testing "should return false when fox alone with corn"
    (is (false? (invalid-left-right? [:fox :corn]))))
  (testing "should return false when fox, you and goose together"
    (is (false? (invalid-left-right? [:fox :goose :you]))))
  (testing "should return false when fox, corn and you together"
    (is (false? (invalid-left-right? [:you :fox :corn]))))
  (testing "should return false when fox, you, goose and corn together"
    (is (false? (invalid-left-right? [:fox :you :corn :goose])))))

(deftest test-invalid-mid?
  (testing "should return true when boat"
    (is (true? (invalid-mid? [:boat]))))
  (testing "should return true when boat and you"
    (is (true? (invalid-mid? [:you :boat]))))
  (testing "should return true when boat, you and fox"
    (is (true? (invalid-mid? [:you :fox :boat]))))
  (testing "should return true when boat, you and corn"
    (is (true? (invalid-mid? [:you :corn :boat]))))
  (testing "should return true when boat, you and goose"
    (is (true? (invalid-mid? [:you :goose :boat]))))
  (testing "should return false when boat, you, goose and corn"
    (is (false? (invalid-mid? [:you :goose :corn :boat]))))
  (testing "should return false when empty"
    (is (false? (invalid-mid? []))))
  (testing "should return false when you"
    (is (false? (invalid-mid? [:you]))))
  (testing "should return false when you and corn"
    (is (false? (invalid-mid? [:you :corn]))))
  (testing "should return false when fox and goose"
    (is (false? (invalid-mid? [:fox :goose])))))
    
(deftest test-generate-left-to-mid-positions
  (testing "should generate correct positions when moving :you left to mid"
    (let [positions (set (generate-left-to-mid-positions [:you :fox :goose :corn] [:boat] []))]
      (is (contains? positions [[:fox :corn] [:boat :you :goose] []]))
      (is (contains? positions [[:goose :corn] [:boat :you :fox] []]))
      (is (contains? positions [[:fox :goose] [:boat :you :corn] []]))
      (is (contains? positions [[:fox :goose :corn] [:boat :you] []])))
    (let [positions (set (generate-left-to-mid-positions [:you :goose] [:boat] [:corn :fox]))]
      (is (contains? positions [[] [:boat :you :goose] [:corn :fox]]))
      (is (contains? positions [[:goose] [:boat :you] [:corn :fox]])))))

;test valid-position?

(comment
  (deftest test-river-crossing-plan
    (let [crossing-plan (map (partial map set) (river-crossing-plan))]
      (testing "you begin with the fox, goose and corn on one side of the river"
        (is (= [#{:you :fox :goose :corn} #{:boat} #{}]
              (first crossing-plan))))
      (testing "you end with the fox, goose and corn on one side of the river"
        (is (= [#{} #{:boat} #{:you :fox :goose :corn}]
              (last crossing-plan))))
      (testing "things are safe"
        (let [left-bank (map first crossing-plan)
              right-bank (map last crossing-plan)]
          (testing "the fox and the goose should never be left alone together"
            (is (empty?)
                (filter #(= % #{:fox :goose}) (concat left-bank right-bank))))
          (testing "the goose and the corn should never be left alone together"
            (is (empty?)
                (filter #(= % #{:goose :corn}) (concat left-bank right-bank))))))
      (testing "The boat can carry only you plus one other"
        (let [boat-positions (map second crossing-plan)]
          (is (empty?)
              (filter #(> (count %) 3) boat-positions))))
      (testing "moves are valid"
        (let [left-moves (map first crossing-plan)
              middle-moves (map second crossing-plan)
              right-moves (map last crossing-plan)]
          (reduce validate-move left-moves)
          (reduce validate-move middle-moves)
          (reduce validate-move right-moves))))))
