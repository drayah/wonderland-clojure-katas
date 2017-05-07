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
    #(conj [] (nth % 2) (nth % 1) (nth % 0)) ;finally switch left and right
    (generate-left-to-mid-positions right mid left))) ;right-to-mid is same as left-to-mid with left, right switched

;gen moves mid to left and right

(defn generate-positions
  "Generate all valid positions we can reach from a given position
  by performing a valid move"
  [position]
  (let [left (nth position 0)
        mid (nth position 1)
        right (nth position 2)]
    (cond
      (some #{:you} left) (filter valid-position? (generate-left-to-mid-positions left mid right))
      (some #{:you} right) "generate moves from right to mid"
      :else "generate moves from mid to left,right")))

(defn river-crossing-plan 
  "Generate the river crossing"
  []
  start-pos)