(ns aoc23.day3
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(def contents
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines))

(defn nums-and-positions
  [input]
  (let [matcher (re-matcher #"\d+" input)]
    (loop [matches []]
      (if (.find matcher)
        (recur (conj matches {:num (parse-long (.group matcher))
                              :start (.start matcher)
                              :end (.end matcher)}))
        matches))))

(def nums
  (reduce concat (map-indexed
    (fn [idx line]
      (let [matches (nums-and-positions line)]
        (map #(assoc % :line idx) matches)))
    contents)))

(def line-length (count (first contents)))
(def num-lines (count contents))

(defn boundaries [item]
  (let [{line :line start :start end :end} item]
    (filter
      #(and (and (>= (first %) 0) (>= (second %) 0))
            (and (< (first %) num-lines) (< (second %) line-length)))
      (concat [(list line (dec start))]
            [(list line end)]
            (reduce concat (map #(list (list (dec line) %) (list (inc line) %))
                 (range (dec start) (inc end))))))))

(defn is-symbol? [char]
  (let [digits (map str (range 10))]
    (not (or (some #(= % (str char)) digits)
             (= (str char) ".")))))

(defn check-boundaries [item]
  (boolean (some (fn [[i j]] (is-symbol? (nth (nth contents i) j))) (boundaries item))))

(println (reduce + (map #(:num %) (filter check-boundaries nums))))

(defn gear-positions? [item]
  (let [bnds (boundaries item)]
    (filter (fn [[i j]] (= (str (nth (nth contents i) j)) "*")) bnds)))


(defn wtf2 [i j]
  (let [x (nth nums i)
        y (nth nums j)
        gx (set (gear-positions? x))
        gy (set (gear-positions? y))
        both (set/intersection gx gy)]
    (do
      ; (cond (> (count gx) 0) (println gx))
      ; (cond (> (count gy) 0) (println gy))
    (cond (= (count both) 0)
            (do 0)
          :else
            (do (println (format "i = %d, j = %d, matching gear!" i j)) (* (:num x) (:num y)))))))

(defn wtf [i]
  (let [js (range (inc i) (count nums))]
    (map #(wtf2 i %) js)))

(println (reduce + (map #(reduce + (wtf %)) (range (count nums)))))
