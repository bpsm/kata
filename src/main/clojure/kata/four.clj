(ns kata.four
  (:use clojure.java.io))

(def INTEGER #(Integer/valueOf %))
(def STRING identity)

(defn half-open-range [min max]
  {:pre [(<= min max)]}
  {:min min :max max})

(defn field [name col-min col-max type]
  {name {:pos (half-open-range col-min col-max) :type type}})

(defn data-declaration
  [url & fields]
  {:pre [(zero? (rem (count fields) 4))]}
  {:url url
   :fields (into {}
                 (map #(apply field %)
                      (partition 4 fields)))})

(defn parse-record [dd line]
  (into {}
        (for [[name {{:keys [min max]} :pos, parse :type}] (:fields dd)]
          {name (-> line
                    (.substring min max)
                    (.trim)
                    parse)})))

(defn maybe-parse-record [dd line]
  (try (parse-record dd line)
       (catch Exception _ nil)))

(defn parse-records [dd]
  (filter identity
          (map #(maybe-parse-record dd %)
               (-> dd :url reader line-seq))))

(defn fspread [fld1 fld2]
  (fn [r]
    (Math/abs (- (get r fld1)
                 (get r fld2)))))

(defn min-by [f s]
  (apply min-key f s))

(defn with-smallest-spread
  [dd fresult fld1 fld2]
  (fresult (min-by (fspread fld1 fld2)
                   (parse-records dd))))

(def weather
     (data-declaration (resource "kata/four/weather.dat")
                       :day 0 4 INTEGER
                       :max-temp 6 8 INTEGER
                       :min-temp 12 14 INTEGER))

(defn day-with-smallest-temperature-spread []
  (with-smallest-spread weather :day :max-temp :min-temp))

(def football
     (data-declaration (resource "kata/four/football.dat")
                       :team 7 22 STRING
                       :for 43 45 INTEGER
                       :against 50 52 INTEGER))

(defn team-with-smallest-for-against-spread []
  (with-smallest-spread football :team :for :against))





