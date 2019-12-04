(ns adventofcode19.day3
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "./inputs/day3.txt")
      (clojure.string/split #"\n")))

(defn abs-val
  [vl]
  (if (> 0 vl) (- vl) vl))

(defn apply-step
  [position step]
  (merge-with + position step))

(defn get-steps
  [input]
  (str/split input #","))

(defn get-step-count
  [step]
  (let [step-count (Integer. (re-find #"\d+" step))]
    (case (keyword (re-find #"[A-Za-z]" step))
      :R {:x step-count}
      :L {:x (- step-count)}
      :U {:y step-count}
      :D {:y (- step-count)})))

(defn get-lines
  [path]
  (partition 2 1 path))

(defn get-path
  [paths]
  (let [initial-position {:x 0 :y 0}]
    (reductions apply-step initial-position paths)))

(defn vertical-line?
  [line]
  (let [second-pt (second line)
        first-pt (first line)]
    (= (:x second-pt) (:x first-pt))))

(defn intersection
  [line-one line-two]
  (if (vertical-line? line-one)
    {:x (:x (first line-one))
     :y (:y (first line-two))}
    {:x (:x (first line-two))
     :y (:y (first line-one))}))

(defn between-x
  [line pt]
  (let [x-coords (sort (map :x line))]
    (< (first x-coords) (:x pt) (last x-coords))))

(defn between-y
  [line pt]
  (let [y-coords (sort (map :y line))]
    (< (first y-coords) (:y pt) (last y-coords))))

(defn valid-intersection
  [line-one line-two]
  (let [pt (intersection line-one line-two)]
    (if (vertical-line? line-one)
      (when (and (between-x line-two pt)
                 (between-y line-one pt))
        pt)
      (when (and (between-x line-one pt)
                 (between-y line-two pt))
        pt))))

(defn distance-from-port
  [pt]
  (+ (abs-val (:x pt))
     (abs-val (:y pt))))

(defn get-number-of-steps
  [line line-path intersect]
  (reduce (fn [acc path]
            (let [pt2 (second path)
                  pt (first path)
                  y-steps (abs-val (- (:y pt2) (:y pt)))
                  x-steps (abs-val (- (:x pt2) (:x pt)))]
              (if (= line path)
                (reduced (if (= (:y (second path)) (:y (first path)))
                           (+ acc (- (:x (first path))
                                     (:x intersect)))
                           (+ acc (- (:y (first path))
                                     (:y intersect)))))
                (+ acc y-steps x-steps)))) 0 line-path))

(defn day-three
  []
  (let [#_#_input ["R8,U5,L5,D3"
                   "U7,R6,D4,L4"]
        wire-one-lines (->> (first input)
                            get-steps
                            (map get-step-count)
                            get-path
                            get-lines)
        wire-two-lines (->> (second input)
                            get-steps
                            (map get-step-count)
                            get-path
                            get-lines)
        intersections (remove nil? (for [line-from-one wire-one-lines
                                         line-from-two wire-two-lines]
                                     (valid-intersection line-from-one line-from-two)))
        lines-creating-intersections (reduce (fn [acc [one two]]
                                               (if (valid-intersection one two)
                                                 (conj acc [one two (valid-intersection one two)])
                                                 acc))
                                             []
                                             (for [line-from-one wire-one-lines
                                                   line-from-two wire-two-lines]
                                               [line-from-one line-from-two]))]
    [(apply min (for [[l1 l2 pt] lines-creating-intersections]
                  (+ (get-number-of-steps l1 wire-one-lines pt)
                     (get-number-of-steps l2 wire-two-lines pt))))
     (apply min (map distance-from-port intersections))]))


(day-three)


