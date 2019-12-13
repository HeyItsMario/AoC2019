(ns adventofcode19.day12
  (:require [clojure.string :as str]))

(def input (slurp "/Users/crdva/Projects/AoC2019/src/adventofcode19/inputs/day12.txt"))

(defn create-moons
  [input]
  (->> (str/split-lines input)
       (map #(re-seq #"-?\d+" %))
       flatten
       (map #(Integer. %))
       (partition 3)
       (mapv #(vector (into [] %) [0 0 0]))))

(def moons (atom (create-moons input)))

(defn apply-gravity-on-positions
  [pos1 pos2 v1 v2]
  (cond
    (> pos1 pos2) [(dec v1) (inc v2)]
    (< pos1 pos2) [(inc v1) (dec v2)]
    :else [v1 v2]))

(defn apply-gravity
  [m1 m2]
  (let [[pos1 vel1]  m1
        [pos2 vel2]  m2
        updated-vels (mapv (fn [p1 p2 v1 v2]
                            (apply-gravity-on-positions p1 p2 v1 v2))
                          pos1 pos2 vel1 vel2)]
    [[pos1 [(get-in updated-vels [0 0])
            (get-in updated-vels [1 0])
            (get-in updated-vels [2 0])]]
     [pos2 [(get-in updated-vels [0 1])
            (get-in updated-vels [1 1])
            (get-in updated-vels [2 1])]]]))

(defn apply-velocity
  [[pos vel]]
  (mapv + pos vel))

(apply-velocity [[1 1 1] [1 2 3]])

;; Apply gravitaional effect on the velocities
(defn part1
  [steps]
  (doseq [_ (range steps)]
    (doseq [x (range 4)]
      (doseq [y (range (inc x) 4)]
        (let [m1 (get @moons x)
              m2 (get @moons y)
              [[_ v1] [_ v2]] (apply-gravity m1 m2)]
          (swap! moons assoc-in [x 1] v1)
          (swap! moons assoc-in [y 1] v2))))
    (doseq [x (range 4)]
      (let [pos (apply-velocity (get @moons x))]
        (swap! moons assoc-in [x 0] pos))))
  (->> (map (fn [[ps vs]]
              (* (reduce + 0 (map #(Math/abs %) ps))
                 (reduce + 0 (map #(Math/abs %) vs)))) @moons)
       (reduce + 0)))

(defn part2
  [_]
  (let [configs #{@moons}
        count 0]
    (loop [initial @moons
           count 1]

      (println "A-z: " (get-in @moons [0 0 2]))
      ;; Apply gravity
      (doseq [x (range 4)]
        (doseq [y (range (inc x) 4)]
          (let [m1 (get @moons x)
                m2 (get @moons y)
                [[_ v1] [_ v2]] (apply-gravity m1 m2)]
            (swap! moons assoc-in [x 1] v1)
            (swap! moons assoc-in [y 1] v2))))
      ;; Apply velocities
      (doseq [x (range 4)]
        (let [pos (apply-velocity (get @moons x))]
          (swap! moons assoc-in [x 0] pos)))

      (if (or (= count 1000)(= initial @moons))
        count
        (recur (conj configs @moons) (inc count)))
      )))


(part2 :start)

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(gcd 28 44)

(gcd 6 4)
