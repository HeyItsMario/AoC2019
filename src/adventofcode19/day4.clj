(ns adventofcode19.day4)

(def start-r 347312)

(def end-r 805915)


(defn six-digit-number?
  [n]
  (= (count (clojure.string/split (str n) #"")) 6))

(defn decreasing?
  [n]
  (->> (clojure.string/split (str n) #"")
       (map #(Integer. %))
       (apply <=)))

(defn two-adjacent?
  [n]
  (boolean (re-find #"(\d)\1" (str n))))

(defn same-digit-group-count?
  [n]
  (let [groups (vals (frequencies (clojure.string/split (str n) #"")))]

    (boolean (some #(= 2 %) groups))))

(defn day4
  []
  (reduce (fn [acc n]
            (if (and (six-digit-number? n)
                     (decreasing? n)
                     (two-adjacent? n)
                     (same-digit-group-count? n))
              (inc acc)
              acc)) 0 (range start-r (inc end-r))))

(day4)












