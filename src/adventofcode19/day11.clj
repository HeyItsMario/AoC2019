(ns adventofcode19.day11
  (:require [clojure.string :as str]
            [adventofcode19.day5 :as intcode]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "/Users/mcordova/Projects/adventofcode19/src/adventofcode19/inputs/day11.txt"))

(str/split input #"(,|\n)")

(-> (intcode/create-program [1102 2 2 3 4 3])
    (intcode/operate)
    intcode/operate)
