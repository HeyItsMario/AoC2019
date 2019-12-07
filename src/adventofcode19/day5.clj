(ns adventofcode19.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

#_(def input
  (->> (-> (io/resource "./inputs/day5.txt")
           slurp
           (clojure.string/split #",|\n"))
       (mapv #(Integer. %))))

(def input ["3,1,99"])


(defn parse-mode
  [modes]
  {:modes (str/split (str/join modes) #"")})

(defn parse-op
  [op]
  {:op (str/join op)})

(defn parse-instruction
  [instruction]
  (let [[modes op] (split-at 3 (format "%05d" instruction))
        op (parse-op op)
        modes (parse-mode modes)]
    (merge op modes)))

(defn get-by-mode
  [program value mode]
  (if (= mode "0")
    (get program value)
    value))

(defn show-output
  [program pointer modes params]
  (let [[_ _ mode1] modes
        store-at (get program (+ pointer 1))
        output (get-by-mode program store-at mode1)]
    [program (+ pointer 2) (assoc params :output output :halt true)]))

(defn get-input
  [program pointer modes {:keys [phase signal] :as params}]
  (let [[_ _ mode1] modes
        store-at (get program (+ pointer 1))]
    (if (not (nil? phase))
      [(assoc-in program [store-at] phase)
       (+ pointer 2)
       {:phase nil :signal signal}]
      [(assoc-in program [store-at] signal)
       (+ pointer 2)
       {:phase nil :signal signal}])))

(defn multiply
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1          (get program (+ pointer 1))
        arg2          (get program (+ pointer 2))
        store-at      (get program (+ pointer 3))]
    [(assoc-in program [store-at] (* (get-by-mode program arg1 mode1)
                                     (get-by-mode program arg2 mode2)))
     (+ pointer 4)]))

(defn add
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1          (get program (+ pointer 1))
        arg2          (get program (+ pointer 2))
        store-at      (get program (+ pointer 3))]
    [(assoc-in program [store-at] (+ (get-by-mode program arg1 mode1)
                                     (get-by-mode program arg2 mode2)))
     (+ pointer 4)]))

(defn jump-if-true
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1        (get program (+ pointer 1))
        arg2 (get program (+ pointer 2))]
    (if (not (zero? (get-by-mode program arg1 mode1)))
      [program (get-by-mode program arg2 mode2)]
      [program (+ pointer 3)])))

(defn jump-if-false
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1            (get program (+ pointer 1))
        arg2            (get program (+ pointer 2))]
    (if (zero? (get-by-mode program arg1 mode1))
      [program (get-by-mode program arg2 mode2)]
      [program (+ pointer 3)])))

(defn less-than
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1          (get program (+ pointer 1))
        arg2          (get program (+ pointer 2))
        store-at      (get program (+ pointer 3))
        value (if (< (get-by-mode program arg1 mode1)
                     (get-by-mode program arg2 mode2))
                1
                0)]
    [(assoc-in program [store-at] value)
     (+ pointer 4)]))

(defn equals
  [program pointer modes]
  (let [[_ mode2 mode1] modes
        arg1          (get program (+ pointer 1))
        arg2          (get program (+ pointer 2))
        store-at      (get program (+ pointer 3))
        value (if (= (get-by-mode program arg1 mode1)
                     (get-by-mode program arg2 mode2))
                1
                0)]
    [(assoc-in program [store-at] value)
     (+ pointer 4)]))

(defn operate
  "Should always get a pointer pointing to a valid instruction.
   Executes instruction and returns a vector containing the updated program
   and the pointer pointing to the next instruction as well as any params.
   NOTE: If operation is 99 then returns :halt"
  [program pointer & params]
  (let [instruction (parse-instruction (get program pointer 99))]
    (case (:op instruction)
      "01" (add program pointer (:modes instruction))
      "02" (multiply program pointer (:modes instruction))
      "03" (get-input program pointer (:modes instruction) (first params))
      "04" (show-output program pointer (:modes instruction) (first params))
      "05" (jump-if-true program pointer (:modes instruction))
      "06" (jump-if-false program pointer (:modes instruction))
      "07" (less-than program pointer (:modes instruction))
      "08" (equals program pointer (:modes instruction))
      "99" [:halt :halt]
      [program pointer])))

(defn operate-with-params
  [program pointer params]
  (let [[program pointer p]
        (operate program pointer params)]
    (if (nil? p)
      [program pointer params]
      [program pointer p])))

(defn day5
  []
  (loop [program input
         pointer 0]
    (when-not (= program :halt)
      (let [[program pointer] (operate program pointer)]
        (recur program pointer)))))

(defn run-program
  [program pointer phase signal]
  (loop [program program
         pointer pointer
         params {:phase phase
                 :signal signal}]
    (if (or (= program :halt) (:halt params))
      [program pointer params]
      (let [[program pointer params] (operate-with-params program pointer params)]
        (recur program pointer params)))))


