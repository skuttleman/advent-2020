(ns advent-of-code.day-13
  (:require [advent-of-code.utils.input :as in]
            [clojure.string :as string]))

(defn depart? [bus-id timestamp]
  (zero? (mod timestamp bus-id)))

(defn departure [{:keys [timestamp buses]}]
  (loop [timestamp' timestamp]
    (if-let [bus-ids (seq (filter #(depart? % timestamp') buses))]
      (* (first bus-ids) (- timestamp' timestamp))
      (recur (inc timestamp')))))

(defn ->input []
  (let [[timestamp buses] (line-seq (in/load-input 13))]
    {:timestamp (in/parse-long timestamp)
     :buses     (sort (into () (comp (remove #{"x"}) (map in/parse-long)) (string/split buses #",")))}))

(comment
  ;; part 1
  (departure (->input)))
