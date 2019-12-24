(ns adventofcode19.day12
  (:require [clojure.string :as str]))

(def input (slurp "day12.txt"))

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
  (doseq [step (range steps)]
    (doseq [x (range 4)]
      (doseq [y (range (inc x) 4)]
        (let [m1 (get @moons x)
              m2 (get @moons y)
              [[_ v1] [_ v2]] (apply-gravity m1 m2)]
          (swap! moons assoc-in [x 1] v1)
          (swap! moons assoc-in [y 1] v2))))
    (doseq [x (range 4)]
      (let [pos (apply-velocity (get @moons x))]
        (swap! moons assoc-in [x 0] pos)))

    )
  (->> (map (fn [[ps vs]]
              (* (reduce + 0 (map #(Math/abs %) ps))
                 (reduce + 0 (map #(Math/abs %) vs)))) @moons)
       (reduce + 0)))

(part1 10)

(defn cycle?
  [m1 m2]
  (if (= m1 m2)
    (println "EQUALS: " m1 " and " m2))
  (= m1 m2))

(defn detect-cycles
  [initials moons]
  (-> (->> (map-indexed (fn [idx [_ vel]]
                          [idx (cycle? (get-in initials [idx 1]) vel)]) moons)
           (into {})
           (group-by val))
      (get true)))

(defn add-to-orbits
  [orbits [m a]]
  (conj orbits [m a]))

(defn detect-orbits
  [moons orbits step]
  (remove nil? (for [axis (range 3)]
                 (let [a (get-in moons [0 1 axis])
                       b (get-in moons [1 1 axis])
                       c (get-in moons [2 1 axis])
                       d (get-in moons [3 1 axis])]
                   (when (= a b c d 0)
                     (when (not (contains? orbits axis))
                       (println "Moons axis " axis " is 0 at step: " step)
                       axis))))))

(defn part2
  [_]
  (loop [initial-moons @moons
         orbits #{}
         cnt 1]
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

    ;; Finished step
    (let [poss-orbs (detect-orbits @moons orbits cnt)
          orbits (if (seq poss-orbs)
                   (apply conj orbits poss-orbs)#_(reduce #(add-to-orbits %1 %2) orbits poss-orbs)
                   orbits)]

      (if (or (= cnt 30000000)
              (= 3 (count orbits)))
        [orbits cnt]
        (recur initial-moons orbits (inc cnt))))))


(apply conj #{} [[nil true]])

(part2 :statr)

