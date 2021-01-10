(ns advent-of-code.day-7
  (:require [advent-of-code.utils.input :as in]
            [clojure.string :as string]))

(defn ->color [s]
  (keyword (string/replace s #"\s" "-")))

(defn ->content [desc]
  (let [[_ count color] (re-find #"(\d+) (\w+ \w+)" desc)]
    [(->color color) (in/parse-long count)]))

(defn ->contents [description]
  (into {} (map ->content) (string/split description #",\s+")))

(defn ->rules [line]
  (let [[_ color contents] (re-matches #"^(\w+ \w+) bags contain (.+)\.$" line)]
    [(->color color)
     (if (= "no other bags" contents)
       {}
       (->contents contents))]))

(defn holders [input color]
  (->> input
       (filter (fn [[_ m]]
                 (pos? (get m color 0))))
       (map first)
       set))

(defn outermost [input color]
  (let [holders (holders input color)]
    (into holders (mapcat #(outermost input %)) holders)))

(def min-bags (memoize (fn [input color]
                         (reduce (fn [total [bag amt]]
                                   (+ total amt (* amt (min-bags input bag))))
                                 0
                                 (get input color)))))

(comment
  (let [input (into {} (in/line-parser (in/load-input 7) ->rules))]
    ;; part 1
    (count (outermost input :shiny-gold))

    ;; part 2
    (min-bags input :shiny-gold)))
