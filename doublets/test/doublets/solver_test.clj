(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))

(deftest test-character-differences
  (testing "sequence for words differing by 1 character"
    (is (= [1 0 0 0]
           (character-differences "book" "look")))
    (is (= [0 1 0 0]
           (character-differences "obok" "olok")))
    (is (= [0 0 1 0]
           (character-differences "oobe" "ooce")))
    (is (= [0 0 0 1]
           (character-differences "ooob" "oool"))))
  (testing "sequence for words differing by 2 characters"
    (is (= [1 0 0 1]
           (character-differences "book" "cool"))))
  (testing "sequence for words with all characters differing"
    (is (= [1 1 1 1]
           (character-differences "book" "apes"))))
  (testing "sequence for same words"
    (is (= [0 0 0 0]
           (character-differences "book" "book")))))

(deftest test-distance
  (testing "should correctly calculate sum of character differences"
    (is (= 1 (distance [1 0 0 0])))
    (is (= 1 (distance [0 1 0 0])))
    (is (= 1 (distance [0 0 1 0])))
    (is (= 1 (distance [0 0 1 0])))
    (is (= 2 (distance [1 0 1 0])))
    (is (= 2 (distance [0 1 0 1])))
    (is (= 3 (distance [1 0 1 1])))
    (is (= 3 (distance [1 1 0 1])))
    (is (= 4 (distance [1 1 1 1])))))

(deftest test-neighbors
  (testing "should have correct neighbors"
    (is (= ["heal"] 
           (neighbors "head" [])))
    (is (= ["heal"] 
           (neighbors "head" ["tell"])))
    (is (= [] 
           (neighbors "head" ["heal" "deal"])))
    (is (= ["head" "teal"]
           (neighbors "heal" [])))
    (is (= ["teal"]
           (neighbors "heal" ["head"])))))

(deftest solver-test
  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))
    (is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))
    (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))
    (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))
  (testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer")))))