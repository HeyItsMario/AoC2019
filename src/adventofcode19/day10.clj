(ns adventofcode19.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "/Users/mcordova/Projects/adventofcode19/src/adventofcode19/inputs/day10.txt"))

(def grid (mapv #(str/split % #"") (str/split-lines input)))

(def asteroids
  (->> (for [row (range (count grid))
             col (range (count grid))
             :when (= "#" (get-in grid [row col]))]
         [col row])
       (remove nil?)
       (into [])))


(defn create-angle
  [[x1 y1] [x2 y2]]
  (let [dy (- y2 y1)
        dx (- x2 x1)
        angle (Math/toDegrees (Math/atan2 dy dx))]
    (+ angle (* (Math/ceil (/ (- angle) 360)) 360))))

(def locations
  (map (fn [a]
         [a (count (distinct (for [b asteroids
                                   :when (not= a b)]
                               (create-angle a b))))]) asteroids))

(defn day10
  []
  (last (sort-by second locations)))

(day10)



