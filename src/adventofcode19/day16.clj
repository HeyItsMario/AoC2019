(ns adventofcode19.day16
  (:require [clojure.string :as str]))


(def repeating-pattern
  [0 1 0 -1])


(defn r-pattern
  [pattern n]
  (let [rp (into [] (mapcat #(repeat n %) pattern))]
    (conj (subvec rp 1) (first rp))))

(r-pattern repeating-pattern 1)

(def input "12345")

#_(def input "59772698208671263608240764571860866740121164692713197043172876418614411671204569068438371694198033241854293277505547521082227127768000396875825588514931816469636073669086528579846568167984238468847424310692809356588283194938312247006770713872391449523616600709476337381408155057994717671310487116607321731472193054148383351831456193884046899113727301389297433553956552888308567897333657138353770191097676986516493304731239036959591922009371079393026332649558536888902303554797360691183681625604439250088062481052510016157472847289467410561025668637527408406615316940050060474260802000437356279910335624476330375485351373298491579364732029523664108987")


(def real-input (str/join (map str (apply concat (take 100 (repeat input))))))

real-input

(def input-signal (mapv #(Integer. %) (str/split input #"")))

input-signal

(def real-signal (mapv #(Integer. %) (str/split input #"")))

real-signal

(defn pattern [n m]
  (let [base [0, 1, 0, -1]
        idx  (base (mod (quot  m n) 4))]
    idx))

(defn output-signal
  [input]
  (let [digit-count (count input)]
    (pmap (fn [n]
            (let [r-patterns (take digit-count (cycle (r-pattern repeating-pattern n)))]
              (-> (reduce  + 0 (pmap * input r-patterns))
                  Math/abs
                  str
                  last
                  str
                  (Integer. )))) (range 1 (inc digit-count)))))

(defn output-sig
  [input]
  (let [digit-count (count input)]
    (->> (pmap (fn [n]
                 (-> (apply + (for [dc (range (count input))]
                                (* (get input dc) (pattern (inc n) (inc dc)))))
                     Math/abs
                     (mod 10)))
               (range 0 digit-count))
         (into []))))


(defn part1
  [input phases]
  (loop [ph phases
         signal input]
    (if (< ph 1)
      signal
      (recur (dec ph) (output-sig signal)))))

(defn move-by
  [input offset]
  (reduce + (subvec input (dec offset))))






(def mov1 (* 100 (reduce + (subvec input-signal 11))))

(def mov2 (* 100 (reduce + (subvec input-signal 10))))

(def mov2 (* 100 (reduce + (subvec input-signal 9))))


(doseq [ph (range 0 10)]
  (println (part1 input-signal ph)))

(defn csn
  "Takes the nth digit where sl is the second to last number and l is the last number"
  [n sl l]
  (mod (+ sl (* n l)) 10))

(defn get-b
  [n i]
  (if (zero? n)
    i
    (mod (+ i (reduce + (for [a (range n)]
                          (csn a 4 5))))
         10)))

(defn get-a
  [n i]
  (if (zero? n)
    i
    (mod (+ i (reduce + (for [b (range n)]
                          (get-b b 2))))
         10)))

(get-b 1 3) ;; should return 2

(get-a 3 1) ;; should return 4

(csn 5 4 5)

