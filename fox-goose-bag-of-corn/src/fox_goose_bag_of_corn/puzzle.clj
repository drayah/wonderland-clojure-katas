(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def end-pos [[[] [:boat] [:fox :goose :corn :you]]])

(defn invalid-left-right?
  "Returns true if given left or right position is not consistent with rules of game"
  [side]
  (let [elements (set side)
        length (count elements)]
    (cond
      (and (= length 2) (contains? elements :fox) (contains? elements :goose)) true
      (and (= length 2) (contains? elements :goose) (contains? elements :corn)) true
      (and (= length 3) (contains? elements :fox) (contains? elements :goose) (contains? elements :corn)) true
      :else false)))

(defn invalid-mid?
  "Returns true if given mid position is not consistent with rules of game"
  [mid]
  (let [elements (set mid)
        length (count elements)]
    (cond
      (or (< length 1) (> length 3) (not (contains? elements :boat))) true
      :else false)))

(defn valid-position?
  "Returns true if given position is consistent with rules of game"
  [position]
  (let [left (nth position 0)
        mid (nth position 1)
        right (nth position 2)]
    (and
      (not (invalid-left-right? left))
      (not (invalid-mid? mid))
      (not (invalid-left-right? right)))))

(defn generate-left-to-mid-positions
  "Generate all positions we can reach by performing moves from left to mid"
  [left mid right]
  (let [options (remove #{:you} left)
        partial-positions (map #(conj [] (remove #{%} options) (conj mid :you %)) options)
        full-positions (map #(conj % right) partial-positions)]
    (conj full-positions (conj [] options (conj mid :you) right))))

(defn generate-right-to-mid-positions
  "Generate all positions we can reach by performing moves from right to mid"
  [left mid right]
  (map 
    #(conj [] (nth % 2) (nth % 1) (nth % 0)) ;switch sides in each generated position 
    (generate-left-to-mid-positions right mid left))) ;right-to-mid is the same as left-to-mid with the sides switched

(defn generate-mid-to-left-positions
  "Generate all positions we can reach by performing moves from mid to left"
  [left mid right]
  (let [options (remove #{:you :boat} mid)
        full-positions (map #(conj [] (conj left :you %) (conj (remove #{%} options) :boat) right) options)]
    (conj full-positions (conj [] (conj left :you) (conj options :boat) right))))
        
(defn generate-mid-to-right-positions
  "Generate all positions we can reach by performing moves from mid to right"
  [left mid right]
  (map 
    #(conj [] (nth % 2) (nth % 1) (nth % 0)) ;switch sides in each generated position 
    (generate-mid-to-left-positions right mid left))) ;mid-to-right is the same as mid-to-left with the sides switched

(defn generate-mid-to-left-right-positions
  "Generate all positions we can reach by performing moves from mid to left and right"
  [left mid right]
  (concat 
    (generate-mid-to-left-positions left mid right)
    (generate-mid-to-right-positions left mid right)))

(defn generate-positions
  "Generate all valid positions we can reach from a given position
  by performing a valid move"
  [position]
  (let [left (nth position 0)
        mid (nth position 1)
        right (nth position 2)]
    (cond
      (some #{:you} left) (filter valid-position? (generate-left-to-mid-positions left mid right))
      (some #{:you} right) (filter valid-position? (generate-right-to-mid-positions left mid right))
      :else "generate moves from mid to left,right")))

(defn river-crossing-plan 
  "Generate the river crossing"
  []
  start-pos)