(ns adventofcode19.day7
  (:require [adventofcode19.day5 :as five]
            [clojure.math.combinatorics :as combo]))



;; wrong answer 2240372680

#_(def input [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
            27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])



(defn amp
  [id program pointer phase signal]
  (let [[program pointer params](five/run-program program pointer phase signal)]
    {:pointer pointer
     :program program
     :id id
     :params params}))

(defn amplify
  [amp signal]
  (let [[program pointer params]
        (five/run-program (:program amp) (:pointer amp) nil signal)]
    {:pointer pointer
     :program program
     :params params
     :id (:id amp)}))

(defn initialize-amps
  [[a b c d e]]
  (let [amp1 (amp :a input 0 a 0)
        amp2 (amp :b input 0 b (-> amp1 :params :output))
        amp3 (amp :c input 0 c (-> amp2 :params :output))
        amp4 (amp :d input 0 d (-> amp3 :params :output))
        amp5 (amp :e input 0 e (-> amp4 :params :output))]
    [amp1 amp2 amp3 amp4 amp5]))

(defn run-setting
  [[a b c d e]]
  (loop [aa a
         ab b
         ac c
         ad d
         ae e]
    (if (= :halt (:program ae))
      (first (remove nil? (map #(get-in % [:params :signal]) [aa ab ac ad ae])))
      (let [ba (amplify aa (-> ae :params :output))
            bb (amplify ab (-> ba :params :output))
            bc (amplify ac (-> bb :params :output))
            bd (amplify ad (-> bc :params :output))
            be (amplify ae (-> bd :params :output))]
        (recur ba bb bc bd be)))))

(defn day7
  []
  (for [settings (combo/permutations #{5 6 7 8 9})]
    (let [amps (initialize-amps settings)]
      (run-setting amps))))


