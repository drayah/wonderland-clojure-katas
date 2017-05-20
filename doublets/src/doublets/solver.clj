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

(defn search
  "Recursively build a path to target word"
  [path target deadends]
  (let [current (first path)]
    (cond
      (nil? current) []                   ;path to target couldn't be found
      (= current target) (reverse path)   ;found target so return path
      :else (let [neighbors (neighbors current (concat path deadends))]
              (if (empty? neighbors) 
                (recur (rest path) target (conj deadends current))
                (let [candidate (first neighbors)]
                  (recur (conj path candidate) target deadends)))))))

(defn doublets 
  "Return links between words"
  [word1 word2]
  (search (conj nil word1) word2 []))