(ns advent-of-code.day-16
  (:require [advent-of-code.utils.input :as in]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn ->rules [line]
  (let [[_ k ranges] (re-find #"([^:]+): (.+)" line)]
    [(keyword (string/replace k #"\s" "-"))
     (map (fn [range]
            (->> range
                 (re-find #"(\d+)-(\d+)")
                 rest
                 (mapv in/parse-long)))
          (string/split ranges #" or "))]))

(defn ->ticket [line]
  (mapv in/parse-long (string/split line #",")))

(defn rules->validator [rules]
  (let [rules (map (fn [[field rules]]
                     [field #(some (fn [[lower upper]] (<= lower % upper)) rules)])
                   rules)]
    (fn [value]
      (reduce (fn [fields [field pred]]
                (cond-> fields
                  (pred value) (conj field)))
              #{}
              rules))))

(defn ->input []
  (let [[rules my-ticket tickets] (string/split (slurp (in/load-input 16)) #"\n\n+")]
    {:rules     (sequence (map ->rules) (string/split-lines rules))
     :my-ticket (->ticket (second (string/split-lines my-ticket)))
     :tickets   (sequence (map ->ticket) (rest (string/split-lines tickets)))}))

(defn possibilities [validator input]
  (loop [[ticket & more] (rest (:tickets input)) field-positions (mapv validator (first (:tickets input)))]
    (if-not ticket
      field-positions
      (let [next-positions (map validator ticket)]
        (recur more
               (cond->> field-positions
                        (every? seq next-positions) (mapv set/intersection next-positions)))))))

(defn find-departures [output ticket]
  (->> output
       (filter (comp (partial re-find #"^departure") name first))
       (map (comp ticket second))
       (reduce * 1)))

(defn error-rate [input]
  (let [validator (rules->validator (:rules input))]
    (reduce + 0 (for [ticket (:tickets input)
                      field ticket
                      :when (empty? (validator field))]
                  field))))

(defn identify-fields [input]
  (let [validator (rules->validator (:rules input))]
    (loop [used #{}
           output {}
           [[idx fields] & more] (->> input
                                      (possibilities validator)
                                      (map-indexed vector)
                                      (sort-by (comp count second)))]
      (if idx
        (let [poss (first (set/difference fields used))]
          (recur (conj used poss)
                 (assoc output poss idx)
                 more))
        (find-departures output (:my-ticket input))))))

(comment
  ;; part 1
  (error-rate (->input))

  ;; part 2
  (identify-fields (->input)))
