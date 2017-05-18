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

;defn distance (sum of character-differences)

(defn doublets [word1 word2]
  (println words)
  "make me work")
