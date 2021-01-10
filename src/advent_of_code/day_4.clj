(ns advent-of-code.day-4
  (:require [advent-of-code.utils.input :as in]
            [clojure.string :as string]))

(defn parse-line [line]
  (->> line
       (re-seq #"(\w+):([^\s]+)")
       (into {} (map (fn [[_ k v]]
                       [(keyword k) v])))))

(defn ->lines [rdr]
  (map parse-line (string/split (slurp rdr) #"\n\n+")))

(defn valid? [passport]
  (every? passport [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(defmulti field-valid? (fn [k _] k))

(defmethod field-valid? :byr
  [_ v]
  (<= 1920 (in/parse-long v) 2002))

(defmethod field-valid? :iyr
  [_ v]
  (<= 2010 (in/parse-long v) 2020))

(defmethod field-valid? :eyr
  [_ v]
  (<= 2020 (in/parse-long v) 2030))

(defmethod field-valid? :hgt
  [_ v]
  (let [[_ amt unit] (re-find #"(\d+)(\w+)" v)
        [lower upper] (case (keyword unit)
                        :cm [150 193]
                        :in [59 76])]
    (<= lower (in/parse-long amt) upper)))

(defmethod field-valid? :hcl
  [_ v]
  (re-matches #"#[0-9a-f]{6}" v))

(defmethod field-valid? :ecl
  [_ v]
  (contains? #{:amb :blu :brn :gry :grn :hzl :oth}
             (keyword v)))

(defmethod field-valid? :pid
  [_ v]
  (re-matches #"\d{9}" v))

(defmethod field-valid? :default
  [_ _]
  true)

(defn valider? [passport]
  (and (valid? passport)
       (every? (fn [[k v]]
                 (try (field-valid? k v)
                      (catch Throwable _
                        false)))
               passport)))

(defn solve [pred passports]
  (count (filter pred passports)))

(comment
  (let [input (->lines (in/load-input 4))]
    ;; part 1
    (solve valid? input)

    ;; part 2
    (solve valider? input)))
