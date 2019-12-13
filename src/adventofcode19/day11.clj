(ns adventofcode19.day11
  (:require [clojure.string :as str]
            [adventofcode19.day5 :as intcode]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> (-> (slurp "")
           (str/split #"(,|\n)"))
       (mapv #(Long. %))))


(defn feed-input
  [{:keys [program pointer relative-base heap store-at]} input]
  (let [instructions (when (< store-at (count program))
                       (assoc-in program [store-at] input))
        heap-alt (when (>= store-at (count program))
                   (assoc-in heap [store-at] input))]
    (if (not (nil? instructions))
      (intcode/create-program instructions (+ 2 pointer) relative-base heap [])
      (intcode/create-program program (+ 2 pointer) relative-base heap-alt []))))

(defn get-input
  [[_ pos] grid]
  (let [color (get grid pos :black)]
    (get {:black 0 :white 1} color)))

(defn grid-color
  [outputs]
  (let [[color _] outputs]
    (if (not (or (empty? outputs)
                 (nil? outputs)))
      (get {0 :black 1 :white} color)
      (do (println "OUTPUTS EMPTY: " outputs)
          :white))))

(defn move-bot
  [[face [x y]] outputs]
  (let [[_ turn] outputs]
    (if (empty? outputs)
      (do (println "OUTPUTS EMPTY standing still")
          [face [x y]])
      (case face
        :north (if (= turn 0)
                 [:west [(dec x) y]]
                 [:east [(inc x) y]])
        :south (if (= turn 0)
                 [:east [(inc x) y]]
                 [:west [(dec x) y]])
        :west (if (= turn 0)
                [:south [x (dec y)]]
                [:north [x (inc y)]])
        :east (if (= turn 0)
                [:north [x (inc y)]]
                [:south [x (dec y)]])))))

(defn run-program
  [program]
  (loop [{:keys [halt? need-input?] :or {halt? false need-input? false} :as prg-state} (intcode/operate program)]
    (if (or halt? need-input?)
      prg-state
      (recur (intcode/operate prg-state)))))


(defn run-hull
  []
  (let [program (intcode/operate (intcode/create-program input))]
    (println "INITIAL: " (:heap program))
    (loop [state program
           grid {}
           position [:north [0 0]]]
      (if (:halt? state)
        (do (println "Program halted.")
            {:position position
             :state state
             :grid grid})
        (if (not (:need-input? state))
          (do (println "Program halted doesn't need input.")
              position)
          (let [outputs (:outputs state)
                color (grid-color outputs)
                ;; Color the grid panel on current position. 
                grid (merge grid {(second position) color})
                ;; Move the bot.
                pos (move-bot position outputs)
                ;; Determine the input based on the new position.
                input (get-input pos grid)
                prog (feed-input state input)]
            (recur (run-program prog)
                   grid
                   pos)))))))

(def results (run-hull))



(for [y (range 18 -65 -1)]
  (str/join (for [x (range -10 70)]
              (let [tile (get (:grid results) [x y])]
                (case tile
                  :black " "
                  :white "#"
                  nil " ")))))



