(ns day2
  (:require [clojure.string :as str]))

(defn parse-col [col]
  (let [[numstr colour] (str/split col #" ")
        num (Integer/parseInt numstr)]
    (cond (= colour "green") {:g num}
          (= colour "red") {:r num}
          (= colour "blue") {:b num})))

(defn parse-hand [hand]
  (def defaults {:r 0 :g 0 :b 0})
  (apply merge (conj (map parse-col hand) defaults)))

(defn parse-game [idx line]
  (let [raw-hands (str/split (second (str/split line #": ")) #"; ")
        hands (map #(str/split % #", ") raw-hands)]
    (list (+ idx 1) (map parse-hand hands))))

(defn maximal-hand [hands]
  (let [colours (list :r :g :b)]
    (zipmap colours (map #(apply max (map % hands)) colours))))

(defn valid-bag? [bounds max-hand]
  (let [colours (list :r :g :b)]
    (every? #(<= (% max-hand) (% bounds)) colours)))

(defn valid-game? [bounds game]
  (let [[idx hands] game
        max-hand (maximal-hand hands)]
    (valid-bag? bounds max-hand)))

(def games
  (map-indexed parse-game
    (->> "INPUT_FILE"
         System/getenv
         slurp
         str/split-lines)))

(def bounds
  {:r 12
   :g 13
   :b 14})

(println (format "Problem 1: %d"
  (->> games
       (filter #(valid-game? bounds %))
       (map first)
       (reduce +))))

(println (format "Problem 2: %d"
  (->> games
       (map second)
       (map maximal-hand)
       (map #(apply * (vals %)))
       (reduce +))))


