(ns adventofcode19.day_two)

(def input
  (->> (clojure.string/split (slurp "./inputs/day-two.txt")
                             #",|\n")
       (mapv #(Integer. %))))

(defn restore-program
  [input noun verb]
  (-> (assoc-in input [1] noun)
      (assoc-in [2] verb)))

(defn update-program
  [program position op]
  (let [argn (get program (get program (inc position)))
        argm (get program (get program (+ 2 position)))
        store-at (get program (+ 3 position))]
    (case op
      1 (assoc-in program [store-at] (+ argn argm))
      2 (assoc-in program [store-at] (* argn argm))
      program)))

(defn day-two
  [noun verb]
  (let [input (into [] (-> input
                           (restore-program noun verb)))]
    (loop [program input
           counter 0]
      (let [op (get program counter)]
        (if (not= op 99)
          (recur (update-program program counter op)
                 (+ counter 4))
          (update-program program counter op))))))

(defn part-two
  []
  (doall
   (remove nil?
           (for [noun (range 100)
                 verb (range 100)]
             (when (= 19690720
                      (first (day-two noun verb)))
               [noun verb])))))





