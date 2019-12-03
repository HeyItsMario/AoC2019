(ns adventofcode19.day3
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "/Users/crdva/Projects/AoC2019/src/adventofcode19/inputs/day3.txt")
      (clojure.string/split #"\n")))


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

(defn intersect?
  [line-one line-two]
  (not= (vertical-line? line-one)
        (vertical-line? line-two)))

(defn intersection
  [line-one line-two]
  (let [pt {:x nil :y nil}]
    (if (vertical-line? line-one)
      {:x (:x (first line-one))
       :y (:y (first line-two))}
      {:x (:x (first line-two))
       :y (:y (first line-one))})))

(defn valid-intersection
  [line-one line-two]
  (let [pt (intersection line-one line-two)]
    pt
    #_(if (vertical-line? line-one)
      (if (and (< (:y pt) (:y (second line-two)))
               (> (:y pt) (:y (first line-two)))
               (< (:x pt) (:x (second line-one)))
               (> (:x pt) (:x (first line-one))))
        pt)
      (if (and (< (:y pt) (:y (second line-one)))
               (> (:y pt) (:y (first line-one)))
               (< (:x pt) (:x (second line-two)))
               (> (:x pt) (:x (first line-two))))
        pt))))

(defn day-three
  []
  (let [wire-one-lines (->> (first input)
                            get-steps
                            (map get-step-count)
                            get-path
                            get-lines)
        wire-two-lines (->> (second input)
                            get-steps
                            (map get-step-count)
                            get-path
                            get-lines)]
    (remove nil? (for [line-from-one wire-one-lines
                       line-from-two wire-two-lines]
                   (if (intersect? line-from-one line-from-two)
                     (valid-intersection line-from-one line-from-two))))))

(day-three)

(count (distinct (day-three)))

