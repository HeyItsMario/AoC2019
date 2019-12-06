(ns adventofcode19.day6
  (:require [clojure.string :as str]))

(def input
  (clojure.string/split
   (slurp "/Users/crdva/Projects/AoC2019/src/adventofcode19/inputs/day6.txt") #"\n"))



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
   (dfs-traversal g v 1))
  ([g v level]
   (let [direct-orbit-count (count (-> g v :orbitters))
         indirect-orbit-count (* (- level 1) direct-orbit-count)]
     (+ (reduce + 0 (for [orbitter (-> g v :orbitters)]
                      (dfs-traversal g orbitter (inc level))))
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

(defn travel-inspect
  [g start f]
  (let-fn [(travel [g root f]
                   (for [orbitter (-> g root :orbitters)]
                     (f g r))) ]))

(defn common-orbitter
  ([g x y]
   (common-orbitter g :COM #{x y} 1 []))
  ([g root seen exp ancestors]
   (-> (let [orbitters (-> g root :orbitters)
             new-seen (remove (set orbitters) seen)
             ancestors (if (= seen new-seen)
                         ancestors
                         (conj ancestors root))
             ]
         (println "SEEN: " new-seen " anc: " ancestors)
         (if (empty? new-seen)
           ancestors
           (for [orbitter orbitters]
             (common-orbitter g orbitter new-seen exp ancestors))))
       flatten
       first)))

(defn orbital-transfers
  [g start end]
  (let [start-level (first (orbital-level g start))
        end-level (first (orbital-level g end))
        level-diff (Math/abs (- end-level start-level))]

    (if (> end-level start-level)
      (if (orbital-line? end-level start-level)
        level-diff
        (+ 2 level-diff)))
    ))

(defn create-graph
  [input]
  (let [nodes (map #(parse-edge %) input)]
    (reduce (fn [g node]
              (add-node g node))
            graph
            nodes)))

(def sample-input ["COM)B"
                   "B)C"
                   "C)D"
                   "H)E"
                   "E)F"
                   "B)G"
                   "G)H"
                   "D)I"
                   "E)J"
                   "J)K"
                   "K)L"])


(first (orbital-level (create-graph sample-input) :I))

(orbital-transfers (create-graph sample-input) :I :J)

(orbital-line? (create-graph sample-input) :D :J)

(common-orbitter (create-graph sample-input) :J :I)

(keys (create-graph sample-input))




