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
    (is (false? (invalid-left-right? [:fox :you :corn :goose]))))
  (testing "should return false when empty"
    (is (false? (invalid-left-right? [])))))

(deftest test-invalid-mid?
  (testing "just the boat should be valid"
    (is (false? (invalid-mid? [:boat]))))
  (testing "just you and the boat should be valid"
    (is (false? (invalid-mid? [:you :boat]))))
  (testing "you, fox and the boat should be valid"
    (is (false? (invalid-mid? [:you :fox :boat]))))
  (testing "boat, you and corn should be valid"
    (is (false? (invalid-mid? [:you :corn :boat]))))
  (testing "boat, you and goose should be valid"
    (is (false? (invalid-mid? [:you :goose :boat]))))
  (testing "boat, you, goose and corn should be invalid"
    (is (true? (invalid-mid? [:you :goose :corn :boat]))))
  (testing "empty collection should be invalid"
    (is (true? (invalid-mid? []))))
  (testing "just you should be invalid"
    (is (true? (invalid-mid? [:you]))))
  (testing "you and corn should be invalid"
    (is (true? (invalid-mid? [:you :corn]))))
  (testing "fox and goose should be invalid"
    (is (true? (invalid-mid? [:fox :goose])))))
    
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

(deftest test-generate-right-to-mid-positions
  (testing "should generate correct positions when moving :you right to mid"
    (let [positions (set (generate-right-to-mid-positions [] [:boat] [:fox :you :corn :goose]))]
      (is (contains? positions [[] [:boat :you] [:fox :corn :goose]]))
      (is (contains? positions [[] [:boat :you :fox] [:corn :goose]]))
      (is (contains? positions [[] [:boat :you :corn] [:fox :goose]]))
      (is (contains? positions [[] [:boat :you :goose] [:fox :corn]])))
    (let [positions (set (generate-right-to-mid-positions [:goose :fox] [:boat] [:you :corn]))]
      (is (contains? positions [[:goose :fox] [:boat :you] [:corn]]))
      (is (contains? positions [[:goose :fox] [:boat :you :corn] []])))))

(deftest test-generate-mid-to-left-positions
  (testing "should generate correct positions when moving :you mid to left"
    (let [positions (set (generate-mid-to-left-positions [:goose] [:boat :you :corn] [:fox]))]
      (is (contains? positions [[:goose :you] [:boat :corn] [:fox]]))
      (is (contains? positions [[:goose :you :corn] [:boat] [:fox]])))))

(deftest test-generate-mid-to-right-positions
  (testing "should generate correct positions when moving :you mid to right"
    (let [positions (set (generate-mid-to-right-positions [:goose] [:boat :you :corn] [:fox]))]
      (is (contains? positions [[:goose] [:boat] [:fox :you :corn]]))
      (is (contains? positions [[:goose] [:boat :corn] [:fox :you]])))))

(deftest test-generate-mid-to-left-right-positions
  (testing "should generate correct positions when moving :you mid to left and right"
    (let [positions (set (generate-mid-to-left-right-positions [:goose] [:boat :you :corn] [:fox]))]
      (is (contains? positions [[:goose :you] [:boat :corn] [:fox]]))
      (is (contains? positions [[:goose :you :corn] [:boat] [:fox]]))
      (is (contains? positions [[:goose] [:boat] [:fox :you :corn]]))
      (is (contains? positions [[:goose] [:boat :corn] [:fox :you]])))))

(deftest test-valid-position?
  (testing "should return true for valid positions"
    (is (true? (valid-position? [[:fox :corn] [:boat :you] [:goose]])))
    (is (true? (valid-position? [[] [:boat] [:you :fox :goose :corn]])))
    (is (true? (valid-position? [[:fox] [:boat] [:goose :you :corn]])))
    (is (true? (valid-position? [[:you :goose] [:boat] [:corn :fox]])))
    (is (true? (valid-position? [[] [:boat :you :goose] [:corn :fox]])))
    (is (true? (valid-position? [[:goose] [:boat :you] [:corn :fox]])))
    (is (true? (valid-position? [[] [:boat :you :goose] [:fox :corn]]))))
  (testing "should return false for invalid positions"
    (is (false? (valid-position? [[] [] []])))
    (is (false? (valid-position? [[:fox :goose] [:boat] [:you :corn]])))
    (is (false? (valid-position? [[:fox :goose] [:boat :you :corn] []])))
    (is (false? (valid-position? [[] [:boat :you :fox] [:goose :corn]])))
    (is (false? (valid-position? [[:corn :goose] [:boat :you :fox] []])))
    (is (false? (valid-position? [[:fox :goose :corn] [:boat :you] []])))
    (is (false? (valid-position? [[] [:boat :you] [:fox :corn :goose]])))
    (is (false? (valid-position? [[:goose :fox] [:boat :you] [:corn]])))
    (is (false? (valid-position? [[:goose :fox] [:boat :you :corn] []])))))

(deftest test-normalized-position
  (testing "should transform a vector of vectors to a vector of sets"
    (is (= [#{:b :a} #{:d :c} #{:e}] (normalized-position [[:a :b] [:c :d] [:e]])))
    (is (= [#{:a :b} #{:c :d} #{}] (normalized-position [[:a :b] [:c :d] []])))))

(deftest test-next-position-for-route
  (testing "should return next position from list of positions"
    (is (= [[:x] [:y] [:z]] 
           (next-position-for-route 
             [[[:x] [:y] [:z]] [[:b :a] [] [:d]]] 
             [[[:a :b] [] [:d]] [[:c :y] [:e] [:f]]]
             [])))
    (is (= [[:a] [:b] [:c]]
           (next-position-for-route
             [[[:x] [:y] [:z]] [[:b :a] [] [:d]] [[:a] [:b] [:c]]]
             [[[:a :b] [] [:d]] [[:c :y] [:e] [:f]]] 
             [[[:x] [:y] [:z]]])))))

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
          (is (empty?
               (filter #(= % #{:fox :goose}) (concat left-bank right-bank)))))
        (testing "the goose and the corn should never be left alone together"
          (is (empty?
               (filter #(= % #{:goose :corn}) (concat left-bank right-bank)))))))
    (testing "The boat can carry only you plus one other"
      (let [boat-positions (map second crossing-plan)]
        (is (empty?
             (filter #(> (count %) 3) boat-positions)))))
    (testing "moves are valid"
      (let [left-moves (map first crossing-plan)
            middle-moves (map second crossing-plan)
            right-moves (map last crossing-plan)]
        (reduce validate-move left-moves)
        (reduce validate-move middle-moves)
        (reduce validate-move right-moves)))))