(ns advent-of-code.day-11
  (:require [advent-of-code.utils.input :as in]))

(def swap-seat
  {:occupied  :available
   :available :occupied})

(defn move [y x]
  (fn [[col row]]
    [(+ col y) (+ row x)]))

(defn adjacencies [input [col row]]
  (->> [(move -1 -1)
        (move -1 0)
        (move -1 1)
        (move 0 -1)
        (move 0 1)
        (move 1 -1)
        (move 1 0)
        (move 1 1)]
       (map #(% [col row]))
       (keep input)
       frequencies
       (merge {:available 0 :occupied 0})))

(defn ->rule [min]
  (fn [val adj]
    (or (and (= :available val) (zero? (:occupied adj)))
        (and (= :occupied val) (<= min (:occupied adj))))))

(defn to-swap [input seen rule]
  (for [[[col row] val] input
        :when (rule val (seen input [col row]))]
    [col row]))

(defn swap-seats [input swaps]
  (reduce (fn [input seat]
            (update input seat swap-seat))
          input
          swaps))

(defn end-occupied [input seen rule]
  (if-let [swaps (seq (to-swap input seen rule))]
    (recur (swap-seats input swaps) seen rule)
    (:occupied (frequencies (vals input)) 0)))

(defn ->input [grid]
  (into {} (for [col (range (count grid))
                 row (range (count (first grid)))]
             [[col row] (get-in grid [col row])])))

(comment
  (let [input (->input (in/grid-parser (in/load-input 11)
                                       {\L :available
                                        \# :occupied
                                        \. :floor}))]
    ;; part 1
    (end-occupied input adjacencies (->rule 4))))
