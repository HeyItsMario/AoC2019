(ns adventofcode19.day15
  (:require [clojure.string :as str]
            [adventofcode19.day5 :as intcode]
            [clojure.math.combinatorics :as combo]))

(def input
  (->> (-> (slurp "day15.txt")
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

;; Helper functions
;; Given two points, determins what step you need to take
;; In order to get to p2
(defn get-direction
  [[x1 y1] [x2 y2]]
  (if (= y1 y2)
    (if (> x1 x2)
      3
      4)
    (if (> y1 y2)
      2
      1)))

(get-direction [0 0] [-1 0])

(defn get-position-after-move
  [[x y] move]
  (case move
    1 [x (inc y)]
    2 [x (dec y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn get-possible-paths
  [position]
  (mapv (partial get-position-after-move position) [1 2 3 4]))

(get-possible-paths [0 0])

(defn run-program
  [program]
  (loop [{:keys [halt? need-input?] :or {halt? false need-input? false} :as prg-state} (intcode/operate program)]
    (if (or halt? need-input?)
      prg-state
      (recur (intcode/operate prg-state)))))

(def grid (atom {[0 0] :path}))

(def rpaths (atom (make-hierarchy)))

(defn dfs
  [pos program]
  (let [children (get-possible-paths pos)
        children (remove #(get @grid %) children)]
    (loop [paths children
           program program]
      (if (empty? paths)
        (println "DONE>>>>>>")
        (let [c (first paths)
              step (get-direction pos c)
              prog (run-program (feed-input program step))
              status (-> prog :outputs first)]
          (if (= status 0)
            (swap! grid assoc c :wall)
            (if (= status 1)
              (do (swap! grid assoc c :path)
                  (swap! rpaths derive (symbol (str c)) (symbol (str pos)))
                  (dfs c prog))
              (do (swap! grid assoc c :os)
                  (swap! rpaths derive (symbol (str c)) (symbol (str pos)))
                  (dfs c prog))))
          (if (empty? (rest paths))
            (println "DONE>")
            (recur (rest paths)
                   program)))))))

(defn part1
  []
  ;; Creates the initial and runs program. Should return asking for input.
  (let [program (intcode/operate (intcode/create-program input))]
    (dfs [0 0] program)))

(def results (part1))

(def maaze (for [y (range 21 -20 -1)]
             (str/join (for [x (range -21 20)]
                         (let [loc (get @grid [x y] :empty)]
                           (case loc
                             :wall "#"
                             :path "."
                             :empty "#"
                             :os "O"
                             " "))))))

(doseq [m maaze]
   (println m))

(defn get-children
  [root h]
  (filter (fn [c]
            (contains? (parents h c) root)) (descendants h root)))

(defn get-child-with-least
  [root end h]
  (->> (map (fn [c]
             (let [d (descendants h c)]
               (if (contains? d end)
                 [(count d) c]
                 nil))) (get-children root h))
       (remove nil?)
       sort
       first
       second))

(def directions-to-os
  (->> (loop [root (symbol (str [0 0]))
                      end (symbol (str [12 -14]))
                      moves []
                      step 1]
                 (let [l (get-child-with-least root end @rpaths)]
                   (if (nil? l)
                     (do (println "DONE.")
                         moves)
                     (recur l end (conj moves l) (inc step)))))
       (mapv (comp read-string str))
       (into [[0 0]])
       ((fn [l] (conj l [12 -14])))
       (partition 2 1)
       (mapv (fn [[x y]]
               (get-direction x y)))))

(def cgrid (atom {[12 -14] {:type :os
                            :count 0}}))

(defn move-bot
  [directions prog]
  (reduce #(run-program (feed-input %1 %2)) prog directions))

(defn dfs-with-counts
  [pos program k]
  (let [children (get-possible-paths pos)
        children (remove #(get @cgrid %) children)]
    (loop [paths children
           program program]
      (if (empty? paths)
        (println "DONE>>>>")
        (let [c (first paths)
              step (get-direction pos c)
              prog (run-program (feed-input program step))
              status (-> prog :outputs first)]
          (if (= status 0)
            (swap! cgrid assoc c {:type :wall :count k})
            (if (= status 1)
              (do (swap! cgrid assoc c {:type :path :count k})
                  (dfs-with-counts c prog (inc k)))
              (do (swap! cgrid assoc c {:type :os :count k})
                  (dfs-with-counts c prog (inc k)))))
          (if (empty? (rest paths))
            (println "DONE>")
            (recur (rest paths)
                   program)))))))

(defn part2
  []
  (let [program (intcode/operate (intcode/create-program input))
        program (move-bot directions-to-os program)]
    (dfs-with-counts [12 -14] program 1)))

(def rs (part2))

(sort @cgrid)

(sort (map (comp :count second) (sort @cgrid)))



