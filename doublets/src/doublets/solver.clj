(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn character-differences
  "Given two words w1 and w2 of same length
  returns a seq of 0s and 1s where 0 indicates same characters
  and 1 indicates differing characters"
  [w1 w2]
  (let [w1 (seq w1)
        w2 (seq w2)]
    (map #(if (= %1 %2) 0 1) w1 w2)))

(defn distance
  "Given a sequence of character differences returns the
  total number of differing characters in given seq"
  [diffs]
  (reduce (fn [sum current] (+ sum current)) diffs))

(defn neighbors
  "Returns a sequence of words 1 character difference
  away from given word"
  [word seen]
  (let [same-lengths (filter #(= (count %) (count word)) words)
        candidates (filter #(= (distance (character-differences % word)) 1) same-lengths)]
    (filter #(not (some #{%} seen)) candidates)))

(defn doublets [word1 word2]
  (println words)
  "make me work")