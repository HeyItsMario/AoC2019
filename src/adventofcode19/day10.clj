(ns adventofcode19.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input )

(def grid (mapv #(str/split % #"") (str/split-lines input)))

(count grid)


(def asteroids (remove nil? (for [row (range 33)
                                  col (range 33)]
                              (if (= "#" (get-in grid [row col]))
                                [col row]))))

(count asteroids)

(defn line-of-sight?
  [a b]
  (let [[x1 y1] a
        [x2 y2] b]
    (if (= x2 x1)
      (fn [x y]
        (and (= x x2)
             (or (< y1 y y2)
                 (> y1 y y2))))
      (if (= y2 y1)
        (fn [x y]
          (and (= y y2)
               (or (< x1 x x2)
                   (> x1 x x2))))
        (let [slope (/ (- y2 y1) (- x2 x1))
              c (- y2 (* slope x2))]
          (fn [x y]
            (and (= y (+ c (* slope x)))
                 (or (< x1 x x2)
                     (> x1 x x2))
                 (or (< y1 y y2)
                     (> y1 y y2)))))))))


(def results (for [a asteroids
                   b asteroids]
               [a (when (not= a b)
                    (let [los? (line-of-sight? a b)]
                      (if (empty? (filter true? (map (fn [[x y]]
                                                       (when (not (or (= [x y] a)
                                                                      (= [x y] b)))
                                                         (los? x y))) asteroids)))
                        1
                        0)))]))


(def day10 (for [astr (partition 303 results)]
             [(first (first astr))(->> (map second astr)
                                       (remove nil?)
                                       (reduce +))]))

(last (sort-by second day10))
