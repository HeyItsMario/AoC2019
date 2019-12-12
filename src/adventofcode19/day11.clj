(ns adventofcode19.day11
  (:require [clojure.string :as str]
            [adventofcode19.day5 :as intcode]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> (-> (slurp "/Users/crdva/Projects/AoC2019/src/adventofcode19/inputs/day11.txt")
           (str/split #"(,|\n)"))
       (mapv #(Long. %))))


(defn feed-input
  [program input store-at]
  (assoc-in program [store-at] input))

(defn get-input
  [[face pos] painted]
  (let [color (get painted pos :black)]
    (if (= color :black)
      0
      1)))

(defn move-bot
  [[dir {:keys [x y]}] move]
  (if (= move :initial)
    [:north {:x 0 :y 0}]
    (case dir
      :north (if (= move 0)
               [:west {:x (dec x) :y y}]
               [:east {:x (inc x) :y y}])
      :south (if (= move 0)
               [:east {:x (inc x) :y y}]
               [:west {:x (dec x) :y y}])
      :west (if (= move 0)
              [:south {:x x :y (dec y)}]
              [:north {:x x :y (inc y)}])
      :east (if (= move 0)
              [:north {:x x :y (inc y)}]
              [:south {:x x :y (dec y)}]))))

(defn run-program
  [program]
  (loop [prog program]
    (if (or (= prog :halt)
            (:need-input? prog))
      prog
      (recur (intcode/operate prog)))))

(defn run-hull
  []
  (let [initialized-p (intcode/operate (intcode/create-program input))]
    (loop [program initialized-p
           op [:initial :initial]
           painted {}
           visited #{}
           current-pos [:north {:x 0 :y 0}]]
      (if (:need-input? program)
        (let [pos   (move-bot current-pos (second op))
              _ (println "MOVING TO: " pos)
              input (get-input current-pos painted)
              instructions (feed-input (:program program) input (:store-at program))
              update-program (run-program
                              (intcode/create-program
                               instructions
                               (+ 2 (:pointer program))
                               (:relative-base program)
                               (:heap program)
                               []))
              need-input? (:need-input? update-program)
              paint (first (:outputs update-program))
              visited (conj visited (second pos))
              painted (merge painted {(second pos) paint})]
          (println ":UPDATE: " (dissoc update-program :program))
          (println "input?: " need-input?
                   "\npaint: " paint
                   "\nvisited: " visited
                   "\npainted: " painted)
          (recur update-program
                 (:outputs update-program)
                 painted
                 visited
                 pos))))))

(run-hull)


(feed-input (:program results) -666 (:store-at results))

(loop [program (intcode/create-program input 1)]
  (when (not (or (= program :halt)
                 (:need-input? program)))
    (recur (intcode/operate program))))
