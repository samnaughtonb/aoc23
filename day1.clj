(ns day1
  (:require [clojure.string :as str]))

(def digit-map-simple
  {["0"] 0
   ["1"] 1
   ["2"] 2
   ["3"] 3
   ["4"] 4
   ["5"] 5
   ["6"] 6
   ["7"] 7
   ["8"] 8
   ["9"] 9})

(def digit-map-hard
  {["zero" "0"]  0
   ["one" "1"]   1
   ["two" "2"]   2
   ["three" "3"] 3
   ["four" "4"]  4
   ["five" "5"]  5
   ["six" "6"]   6
   ["seven" "7"] 7
   ["eight" "8"] 8
   ["nine" "9"]  9})

(defn first-posn [input nums]
  (let [raw-posns (map #(str/index-of input %) nums)
        act-posns (filter #(not (nil? %)) raw-posns)]
    (cond (> (count act-posns) 0) (apply min act-posns)
          :else nil)))

(defn first-num [mapping input]
  (defn my-func [acc [matches value]]
    (let [[fst num] acc
          wtf (first-posn input matches)]
      (cond (or (nil? wtf) (>= wtf fst)) acc
            :else [wtf value])))
  (second (reduce my-func
                  [(+ 1 (count input)) nil]
                  mapping)))

(defn reverse-mapping [mapping]
  (zipmap (map #(map str/reverse %) (keys mapping)) (vals mapping)))

(println (reverse-mapping digit-map-simple))
(println (reverse-mapping digit-map-hard))

(defn calibration-value [mapping input]
  (let [fst (first-num mapping input)
        lst (first-num (reverse-mapping mapping) (str/reverse input))]
    (+ (* 10 fst) lst)))

(defn total-calibration-value [contents mapping]
  (->> contents
       (map #(calibration-value mapping %))
       (reduce +)))

(def contents
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines))

(println (format "Problem 1: %d"
                 (total-calibration-value contents digit-map-simple)))

(println (format "Problem 2: %d"
                 (total-calibration-value contents digit-map-hard)))
