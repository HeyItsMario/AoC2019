(ns adventofcode19.day9
  (:require [clojure.string :as str]
            [adventofcode19.day5 :as five]
            [clojure.java.io :as io]))

(def input
  (->> (->  "./inputs/day9.txt"
            slurp
            (clojure.string/split #",|\n"))
       (mapv #(Integer. %))))



(def smaple [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

(def ex1 [1102,34915192,34915192,7,4,7,99,0])

(def test-input [109 5 203 3])


(five/run-program-9 input)

(five/run-program-9 smaple)

(five/run-program-9 [1102,34915192,34915192,7,4,7,99,0])

(five/run-program-9 [104,1125899906842624,99])

(five/run-program-9 [109, -1, 4, 1, 99])

(five/run-program-9 [109, -1, 104, 1, 99])

(five/run-program-9 [109, -1, 204, 1, 99])

(five/run-program-9 [109, 1, 9, 2, 204, -6, 99])

(five/run-program-9 [109, 1, 109, 9, 204, -6, 99])

(five/run-program-9 [109, 1, 209, -1, 204, -106, 99])

(five/run-program-9 [109, 1, 3, 3, 204, 2, 99])

(five/run-program-9 [109, 1, 203, 2, 204, 2, 99])


