(ns adventofcode19.day_one)

;; Make sure to put the correct Filepath.
(def input (slurp "./inputs/day-one.txt"))

(defn fuel-required
  [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn additional-fuel
  [fuel]
  (let [additional-f (fuel-required fuel)]
    (if (< 0 additional-f)
      (+ fuel (additional-fuel additional-f))
      fuel)))

(defn day-one
  []
  (->> (clojure.string/split input #"\n")
       (map #(Integer. %))
       (map #(additional-fuel (fuel-required %)))
       (reduce +)))







