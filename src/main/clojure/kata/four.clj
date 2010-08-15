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
  [url line-min line-max & fields]
  {:pre [(zero? (rem (count fields) 4))
         (<= line-min line-max)]}
  {:url url
   :line (half-open-range line-min line-max)
   :fields (into {} (map #(apply field %) (partition 4 fields)))})

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

(defn data-line-seq [dd]
  (let [mi (-> dd :line :min)
        ma (-> dd :line :max)
        ndrop (dec mi)
        ntake (- ma mi)]
    (->> dd :url reader line-seq
         (drop ndrop)
         (take ntake))))

(defn parse-records [dd]
  (filter identity
          (map #(maybe-parse-record dd %)
               (data-line-seq dd))))

(defn ffield-spread [fld1 fld2]
  (fn [r]
    (Math/abs (- (get r fld1)
                 (get r fld2)))))

(defn min-by [f s]
  (apply min-key f s))

(def weather
     (data-declaration (resource "kata/four/weather.dat") 9 39
                       :day 0 4 INTEGER
                       :max-temp 6 8 INTEGER
                       :min-temp 12 14 INTEGER))

(defn day-with-smallest-temperature-spread []
  (:day
   (min-by (ffield-spread :max-temp :min-temp)
           (parse-records weather))))

(def football
     (data-declaration (resource "kata/four/football.dat") 6 27
                       :team 7 22 STRING
                       :for 43 45 INTEGER
                       :against 50 52 INTEGER))

(defn team-with-smallest-for-against-spread []
  (:team
   (min-by (ffield-spread :for :against)
           (parse-records football))))





