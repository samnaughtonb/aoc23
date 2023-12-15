(ns aoc23.day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn parse-card [line]
  (let [[id sides] (str/split line #":")
        [nums wins] (str/split sides #"\|")
        id (parse-long (last (str/split id #" ")))
        nums (into #{} (filter (comp not nil?)
                               (map parse-long (str/split nums #" "))))
        wins (into #{} (filter (comp not nil?)
                               (map parse-long (str/split wins #" "))))]
    {:id id :nums nums :wins wins}))

(defn count-winning [card]
  (let [{nums :nums wins :wins} card]
    (count (set/intersection nums wins))))

(defn count-points [card]
  (let [winning (count-winning card)
        exp (- winning 1)]
    (cond (> winning 0) (reduce * (repeat exp 2))
          :else 0)))

(defn add-to-totals [cap totals card]
  (let [winning (count-winning card)
        start (inc (:id card))
        end (min (+ start winning) cap)
        nums (range start end)]
    (reduce (fn [totals num] (update totals num #(+ (get totals (:id card) 0) %))) 
            totals 
            nums)))

(defn count-totals [cards]
  (let [n (count cards)
        totals0 (zipmap (range 1 (inc n)) (repeat 1))]
    (reduce (fn [totals card] (add-to-totals n totals card))
            totals0
            cards)))

(def cards
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines
       (map parse-card)))

(println
  (->> cards
       (map count-points)
       (reduce +)))

(println
  (->> cards
       count-totals
       vals
       (reduce +)))
