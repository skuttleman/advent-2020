(ns advent-of-code.day-17
  (:require [advent-of-code.utils.input :as in]))

(defn neighbors [[x y z w]]
  (for [x' (range (dec x) (+ 2 x))
        y' (range (dec y) (+ 2 y))
        z' (range (dec z) (+ 2 z))
        w' (if w
             (range (dec w) (+ 2 w))
             [nil])
        :when (not= [x y z w] [x' y' z' w'])]
    (cond-> [x' y' z'] w' (conj w'))))

(defn ->input [more]
  (let [grid (in/grid-parser (in/load-input 17) {\# :active})
        max-y (count grid)
        max-x (count (first grid))]
    {:active (into #{} (for [col (range max-x)
                             row (range max-y)
                             :let [val (get-in grid [row col])]
                             :when val]
                         (into [col row] more)))
     :min    (into [0 0] more)
     :max    (into [(dec max-x) (dec max-y)] more)}))

(defn instrs [state]
  (let [[x1 y1 z1 w1] (:min state)
        [x2 y2 z2 w2] (:max state)]
    (for [x (range (dec x1) (+ 2 x2))
          y (range (dec y1) (+ 2 y2))
          z (range (dec z1) (+ 2 z2))
          w (if (and w1 w2)
              (range (dec w1) (+ 2 w2))
              [nil])
          :let [coords (cond-> [x y z] w (conj w))
                active? (contains? (:active state) coords)
                adj (count (filter (:active state) (neighbors coords)))]
          :when (or (and active? (not (<= 2 adj 3)))
                    (and (not active?) (= 3 adj)))]
      [(if active? :remove :add)
       coords])))

(defn step [state]
  (->> state
       instrs
       (reduce (fn [state [instr coord]]
                 (cond-> state
                   (= :add instr) (-> (update :active conj coord)
                                      (update :min (partial mapv min coord))
                                      (update :max (partial mapv max coord)))
                   (= :remove instr) (update :active disj coord)))
               state)))

(defn steps [state amt]
  (cond-> state
    (pos? amt) (-> step (recur (dec amt)))))

(defn solve [input]
  (count (:active (steps input 6))))

(comment
  ;; part 1
  (solve (->input [0]))

  ;; part 2
  (solve (->input [0 0])))
