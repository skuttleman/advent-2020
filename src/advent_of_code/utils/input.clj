(ns advent-of-code.utils.input
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn load-input [day]
  (io/reader (io/resource (str "day-" day ".txt"))))

(defn line-parser [rdr f]
  (sequence (map (comp f string/trim)) (line-seq rdr)))

(defn split-parser [rdr re f]
  (sequence (map (comp f string/trim)) (string/split (slurp rdr) re)))

(defn group-parser [rdr f]
  (split-parser rdr "\n\n+" f))

(defn grid-parser [rdr f]
  (transduce (map (partial transduce (map f) conj))
             conj
             (line-seq rdr)))

(defn parse-long
  ([x]
   (Long/parseLong (str x)))
  ([x radix]
   (Long/parseLong x radix)))
