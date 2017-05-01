(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def end-pos [[[] [:boat] [:fox :goose :corn :you]]])

(defn generate-left-to-mid-positions
  "Generate all positions we can reach by 
  performing moves from left to mid"
  [left mid right]
  (let [options (remove #{:you} left)
        left-mid (map #(conj [] (remove #{%} options) (conj mid :you %)) options)]
    (map #(conj % right) left-mid)))
(defn river-crossing-plan 
  "Generate the river crossing"
  []
  start-pos)