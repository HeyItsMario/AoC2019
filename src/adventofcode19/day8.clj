(ns adventofcode19.day8
  (:require [clojure.string :as str]))


  (def input "filepath]")

(defn layer
  [rows]
  {:counts (frequencies (apply concat rows))
   :grid rows})

(defn layers
  [code cols rows]
  (->> (partition cols code)
       (map #(into [] %))
       (partition rows)
       (map #(into [] %))
       (mapv #(layer %))))

(defn image
  [code cols rows]
  {:layers (layers code cols rows)
   :wide cols
   :tall rows})



(defn checksum-layer
  [img]
  (reduce (fn [acc layer]
            (let [zero-count (get (:counts layer) 0)]
              (if (< zero-count (:zero-count acc))
                (merge layer {:zero-count zero-count})
                acc)))
          {:zero-count (* (:wide img) (inc (:tall img)))}
          (:layers img)))

(defn decode
  [img]
  (-> (for [row (range (:tall img))
            coll (range (:wide img))]
        (let [layers (:layers img)]
          (->> (map (fn [layer]
                      (get-in (:grid layer) [row coll])) layers)
               (filter #(not= % 2))
               first)))
      (image (:wide img) (:tall img))))

(def myimage (image input 25 6))

(doseq [row (:grid (first (:layers (decode myimage))))]
  (println (map (fn [pixel]
                  (if (= pixel 1)
                    "*"
                    " "))row)))



