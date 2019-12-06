(ns adventofcode19.day6
  (:require [clojure.string :as str]))

(def input
  (clojure.string/split
   (slurp "./inputs/day6.txt") #"\n"))

(def graph {})

(defn parse-edge
  "An edge comes in the form of `XXX)YYY` such that
   YYY is in direct orbit to XXX.
   RETURNS: a node which is a map in the shape of {:id XXX :orbitters [:YYY]}"
  [edge]
  (let [[id orbitter] (str/split edge #"\)")]
    {:id (keyword id)
     :orbitter (keyword orbitter)}))

(defn add-node
  [g node]
  (let [orbitters (get-in g [(:id node) :orbitters] [])
        node-id (:id node)]
    (assoc g node-id
              {:id node-id
               :orbitters (conj orbitters (:orbitter node))})))

(defn checksum
  ([g v]
   (checksum g v 1))
  ([g v level]
   (let [direct-orbit-count (count (-> g v :orbitters))
         indirect-orbit-count (* (- level 1) direct-orbit-count)]
     (+ (reduce + 0 (for [orbitter (-> g v :orbitters)]
                      (checksum g orbitter (inc level))))
        direct-orbit-count
        indirect-orbit-count))))

(defn orbital-level
  "Take a graph, g, and vertex ,v ,
   Returns the orbital level of v."
  ([g v]
   (orbital-level g :COM v 1))
  ([g root v level]
   (if (= root v)
     level
     (flatten (for [orbitter (-> g root :orbitters)]
                (orbital-level g orbitter v (inc level)))))))

(defn orbital-line?
  [g start end]
  (->> (let [orbitters (-> g start :orbitters)]
         (if orbitters
           (if (some #(= end %) orbitters)
             [true]
             (for [orbitter (-> g start :orbitters)]
               (orbital-line? g orbitter end)))
           [false]))
       (filter true?)
       empty?
       not))

(defn common-ancestor
  ([g x y]
   (common-ancestor g :COM :COM [x y] 1))
  ([g root ancestor orbits level]
   (let [orbit-f (orbital-line? g root (first orbits))
         orbit-l (orbital-line? g root (second orbits))
         ca (if (and orbit-f orbit-l)
              root
              ancestor)
         ancestors (for [orbitter (-> g root :orbitters)]
                     (common-ancestor g orbitter ca orbits (inc level)))]
     (->> (if (not= ca ancestor)
            (conj ancestors {ca level})
            ancestors)
          flatten))))

(defn orbital-transfers
  [g start end]
  (let [start-level (first (orbital-level g start))
        end-level (first (orbital-level g end))
        ca  (first (vals (last (common-ancestor g start end))))]
    (- (+ (Math/abs (- start-level  ca))
          (Math/abs (- end-level  ca)))
       2)))

(defn create-graph
  [input]
  (let [nodes (map #(parse-edge %) input)]
    (reduce (fn [g node]
              (add-node g node))
            graph
            nodes)))

(defn day6
  []
  (orbital-transfers (create-graph input) :YOU :SAN))










