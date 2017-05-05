(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def end-pos [[[] [:boat] [:fox :goose :corn :you]]])

(defn invalid-left-right?
  "Returns true if given left or right position is inconsistent with rules of game"
  [side]
  (let [elements (set side)
        length (count elements)]
    (cond
      (and (= length 2) (contains? elements :fox) (contains? elements :goose)) true
      (and (= length 2) (contains? elements :goose) (contains? elements :corn)) true
      (and (= length 3) (contains? elements :fox) (contains? elements :goose) (contains? elements :corn)) true
      :else false)))

;invalid-mid?

(defn valid-position?
  "Returns true if given position is consistent with rules of game"
  [position]
  (let [left (nth position 0)
        mid (nth position 1)
        right (nth position 2)]
    (and
      (not (invalid-left-right? left))
      (not (invalid-left-right? right)))))
      ;test mid

(defn generate-left-to-mid-positions
  "Generate all positions we can reach by performing moves from left to mid"
  [left mid right]
  (let [options (remove #{:you} left)
        partial-positions (map #(conj [] (remove #{%} options) (conj mid :you %)) options)
        full-positions (map #(conj % right) partial-positions)]
    (conj full-positions (conj [] options (conj mid :you) right))))

;gen moves right to mid

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