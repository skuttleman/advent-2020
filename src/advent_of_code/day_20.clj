(ns advent-of-code.day-20
  (:require [advent-of-code.utils.input :as in]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn last* [coll]
  (if (counted? coll)
    (nth coll (dec (count coll)))
    (last coll)))

(defn parse-group [group]
  (let [[id & more] (string/split-lines group)
        [_ id] (re-find #"Tile (\d+):" id)
        grid (mapv vec more)
        top    (first grid)
        bottom (vec (reverse (last* grid)))
        left   (vec (into () (map first) grid))
        right  (mapv last* grid)]
    {:id    (in/parse-long id)
     :grid  grid
     :sides #{top bottom left right (reverse top) (reverse bottom) (reverse left) (reverse right)}}))

(defn ->input []
  (in/group-parser (in/load-input 20) parse-group))

(defn solve [input]
  (->> (for [tile1 input
             tile2 input
             :when (not= (:id tile1) (:id tile2))
             :when (seq (set/intersection (:sides tile1) (:sides tile2)))]
         #{(:id tile1) (:id tile2)})
       distinct
       (mapcat seq)
       frequencies
       (filter (comp #{2} second))
       (map first)
       (reduce * 1)))

(comment
  ;; part 1
  (solve (->input)))
