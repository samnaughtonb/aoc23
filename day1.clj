(ns aoc23.day1
  (:require [clojure.string :as str]))

(defn first-posn
  "From an input string and a vector of substring against which
  to match, find the first index that matches any of them; if none
  match, return nil"
  [input matches]
  (let [raw-posns (map #(str/index-of input %) matches)
        act-posns (filter #(not (nil? %)) raw-posns)]
    (cond (> (count act-posns) 0) (apply min act-posns)
          :else nil)))

(defn first-num
  "Find the first instance of a number in string input,
  where `mapping` maps a vector of matching strings to an integer"
  [mapping input]
  (second
    (reduce (fn [acc [matches value]]
      (let [[fst num] acc
            posn (first-posn input matches)]
        (cond (or (nil? posn) (>= posn fst)) acc
              :else [posn value])))
      [(+ 1 (count input)) nil] ; initial reduction value
      mapping)))

(defn reverse-mapping
  "Mapping of the form `{ ['hello' 'world'] 69 ... }` are reversed to
  obtain new mappings of the form `{ ['olleh' 'dlrow'] 69 ... }`"
  [mapping]
  (zipmap (map #(map str/reverse %) (keys mapping)) (vals mapping)))

(defn calibration-value
  "Determine first and last matching digits (as per `mapping`) in `input`
  string then 'concatenate', which is equivalent to multiplying the first
  by 10 and summing the second (since they're going to be single digits)"
  [mapping input]
  (let [fst (first-num mapping input)
        lst (first-num (reverse-mapping mapping) (str/reverse input))]
    (+ (* 10 fst) lst)))

(defn total-calibration-value [contents mapping]
  "Sum the 'calibration values' determined from each line of input"
  (->> contents
       (map #(calibration-value mapping %))
       (reduce +)))

(def contents
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines))

(def digit-map-part1
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

(println (format "Problem 1: %d"
                 (total-calibration-value contents digit-map-part1)))

(def digit-map-part2
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

(println (format "Problem 2: %d"
                 (total-calibration-value contents digit-map-part2)))
