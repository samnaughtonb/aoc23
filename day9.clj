(ns aoc23.day9
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map parse-long (str/split line #" ")))

(def contents
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines
       (map parse-line)))

(defn compute-line [xs]
  (let [diffs (map - (rest xs) xs)]
    (if (every? #(= % 0) diffs)
      (last xs)
      (+ (last xs) (compute-line diffs)))))

(println (reduce + (map compute-line contents)))
(println (reduce + (map (comp compute-line reverse) contents)))
