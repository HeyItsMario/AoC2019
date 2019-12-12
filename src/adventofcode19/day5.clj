(ns adventofcode19.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

#_(def input
  (->> (-> (io/resource "./inputs/day5.txt")
           slurp
           (clojure.string/split #",|\n"))
       (mapv #(Integer. %))))

(def input ["3,1,99"])

(def input [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

(defn create-program
  ([input]
   {:program input
    :pointer 0
    :heap {}
    :outputs []
    :relative-base 0})
  ([input pointer off-set heap outputs]
   {:program input
    :pointer pointer
    :heap heap
    :outputs outputs
    :relative-base off-set}))

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

(defn get-by-position
  [{:keys [program heap]} pointer]
  (let [position (get program pointer)]
    (if (< position (count program))
      (get program position)
      (get heap position 0))))

(defn get-by-relative-position
  [{:keys [program heap relative-base]} pointer]
  (let [position (get program pointer)
        rp (+ position relative-base)]
    (if (<  rp (count program))
      (get program rp)
      (get heap rp 0))))

(defn get-by-mode
  [program pointer mode]
  (case (str mode)
    "0" (get-by-position program pointer)
    "1" (get (:program program) pointer)
    "2" (get-by-relative-position program pointer)
    (do (println "non passed")
        (get (:program program) pointer))))

(defn show-output
  [{:keys [program pointer relative-base heap outputs] :as prg} modes params]
  (let [[_ _ mode1] modes
        output (get-by-mode prg (+ pointer 1) mode1)]
    (println "OUTPUT: " output)
    (create-program program (+ pointer 2) relative-base heap (conj outputs output))))

(defn get-input
  [{:keys [program pointer relative-base heap outputs] :as prg} modes {:keys [phase signal] :as params}]
  (let [[_ _ mode1] modes
        store-at (get program (+ pointer 1))
        store-at (if (= (str mode1) "2")
                   (+ store-at relative-base)
                   store-at)]
    (create-program
     (if (< store-at (count program))
       (assoc-in program [store-at] 2)
       program)
     (+ pointer 2)
     relative-base
     (if (< store-at (count program))
       heap
       (assoc-in heap [store-at] 2))
     outputs)))

(defn adder
  [{:keys [program pointer relative-base heap outputs] :as prg} modes f]
  (let [[mode3 mode2 mode1] modes
        arg1          (get-by-mode prg (+ pointer 1) mode1)
        arg2          (get-by-mode prg (+ pointer 2) mode2)
        store-at      (get program (+ pointer 3))
        store-at      (if (= (str mode3) "2")
                        (+ store-at relative-base)
                        store-at)]
    (create-program
     (if (< store-at (count program))
       (assoc-in program [store-at] (f arg1 arg2))
       program)
     (+ pointer 4)
     relative-base
     (if (< store-at (count program))
       heap
       (assoc-in heap [store-at] (f arg1 arg2)))
     outputs)))

(defn jump-if
  [{:keys [program pointer relative-base heap outputs] :as prg} modes f]
  (let [[_ mode2 mode1] modes
        arg1            (get-by-mode prg (+ pointer 1) mode1)
        arg2            (get-by-mode prg (+ pointer 2) mode2)]
    (if (f arg1)
      (create-program program arg2 relative-base heap outputs)
      (create-program program (+ pointer 3) relative-base heap outputs))))

(defn set-when
  [{:keys [outputs program pointer relative-base heap] :as prg} modes f]
  (let [[mode3 mode2 mode1] modes
        arg1          (get-by-mode prg (+ pointer 1) mode1)
        arg2          (get-by-mode prg (+ pointer 2) mode2)
        store-at      (get program (+ pointer 3))
        store-at      (if (= (str mode3) "2")
                        (+ store-at relative-base)
                        store-at)
        value (if (f arg1 arg2) 1 0)]
    (create-program
     (if (< store-at (count program))
       (assoc-in program [store-at] value)
       program)
     (+ pointer 4)
     relative-base
     (if (< store-at (count program))
       heap
       (assoc-in heap [store-at] value))
     outputs)))

(defn base-off-set
  [{:keys [program pointer relative-base heap outputs] :as prg} modes]
  (let [[_ _ mode1] modes
        arg1 (get-by-mode prg (+ pointer 1) mode1)]
    (create-program program (+ pointer 2) (+ relative-base arg1) heap outputs)))

(defn operate
  [{:keys [program pointer] :as prg} & params]
  (let [instruction (parse-instruction (get program pointer 99))
        modes (:modes instruction)]
    (condp = (:op instruction)
      "01" (adder prg modes +)
      "02" (adder prg modes *)
      "03" (get-input prg modes (first params))
      "04" (show-output prg modes (first params))
      "05" (jump-if prg modes (comp not zero?))
      "06" (jump-if prg modes zero?)
      "07" (set-when prg modes <)
      "08" (set-when prg modes =)
      "09" (base-off-set prg modes)
      "99" :halt
      :else prg)))

(defn run-program-9
  [input]
  (let [prg (create-program input)]
    (loop [program prg]
      (when-not (= program :halt)
        (let [result (operate program)]
          (recur result))))))

#_(run-program-9 [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

(defn operate-with-params
  [program pointer params]
  (let [[program pointer p]
        (operate program pointer params)]
    (if (nil? p)
      [program pointer params]
      [program pointer p])))

(defn day5
  []
  (let [program (create-program input)]
    (loop [program program
           pointer 0]
      (when-not (= program :halt)
        (let [[program pointer] (operate program pointer)]
          (recur program pointer))))))

(defn run-program
  "Day 6."
  [program pointer phase signal]
  (loop [program program
         pointer pointer
         params {:phase phase
                 :signal signal}]
    (if (or (= program :halt) (:halt params))
      [program pointer params]
      (let [[program pointer params] (operate-with-params program pointer params)]
        (recur program pointer params)))))
