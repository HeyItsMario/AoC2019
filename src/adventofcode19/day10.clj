(ns adventofcode19.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (slurp ""))

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
        angle (+ 90 (Math/toDegrees (Math/atan2 dy dx)))]
    (+ angle (* (Math/ceil (/ (- angle) 360)) 360))))

(defn get-distance
  [[sx sy] [x y]]
  (let [dy (- y sy)
        dx (- x sx)]
    (Math/hypot dx dy)))

(time (def locations
        (mapv (fn [a]
               [a (count (distinct (for [b asteroids
                                         :when (not= a b)]
                                     (create-angle a b))))]) asteroids)))


(defn day10-part1
  []
  (last (sort-by second locations)))

(def station (first (day10-part1)))


(def laser-targets
  (for [a asteroids
        :when (not= station a)]
    {:location a
     :distance (get-distance station a)
     :angle (create-angle station a)}))

(def order-of-explosions
  (sort-by key (group-by :angle (sort-by :distance laser-targets))))


(time (loop [asteroids (map second order-of-explosions)
             count (atom 0)]
        (if (seq asteroids)
          (for [a asteroids]
            (when (not (nil? (first a)))
              (swap! count inc)))
          (recur (remove empty? (map rest asteroids)) count))))

