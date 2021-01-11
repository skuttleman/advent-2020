(ns advent-of-code.utils.input
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn load-input [day]
  (io/reader (io/resource (str "day-" day ".txt"))))

(defn line-parser [rdr f]
  (sequence (map f) (line-seq rdr)))

(defn group-parser [rdr f]
  (sequence (map f) (string/split (slurp rdr) #"\n\n+")))

(defn grid-parser [rdr f]
  (transduce (map (partial transduce (map f) conj))
             conj
             (line-seq rdr)))

(defn parse-long [x]
  (Long/parseLong x))
