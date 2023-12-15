(ns aoc23.day2
  (:require [clojure.string :as str]))

(defn parse-col
  "Create data structure (map) from an input string, e.g.
  the string '7 green' would go to {:g 7}"
  [col]
  (let [[numstr colour] (str/split col #" ")
        num (parse-long numstr)]
    (case colour
      "green" {:g num}
      "red" {:r num}
      "blue" {:b num})))

(defn parse-hand
  "Map a hand (i.e. a list of strings) to a map representing
  the contents thereof, defaulting to zero for colours that
  don't appear in the hand, e.g. '7 green, 2 blue' would map
  to {:r 0 :g 7 :b 2}"
  [hand]
  (let [defaults {:r 0 :g 0 :b 0}]
    (apply merge (conj (map parse-col hand) defaults))))

(defn parse-game
  "Given an enumerated line string, return a data structure of
  the form (123 ({:r 42 ...} {:r 69 ...})) representing the game
  number and the list of hands in the game"
  [idx line]
  (let [raw-hands (str/split (second (str/split line #": ")) #"; ")
        hands (map #(str/split % #", ") raw-hands)]
    (list (+ idx 1) (map parse-hand hands))))

(defn maximal-hand
  "Find colour-wise maximum given a list of hands"
  [hands]
  (let [colours (list :r :g :b)]
    (zipmap colours (map #(apply max (map % hands)) colours))))

(defn valid-bag?
  "Test whether or not a hand satisfies the colour-wise maximum
  constraints specified by `bounds`"
  [bounds hand]
  (let [colours (list :r :g :b)]
    (every? #(<= (% hand) (% bounds)) colours)))

(defn valid-game?
  "Test whether or a game satisfies the colour-wise maximum
  constraints specified by `bounds`, that is, whether or not
  the colour-wise maximal hand satisfies the bounds"
  [bounds game]
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

(defn total-idcs-valid-games
  "(Part 1) Filter the games by which are valid under the specified
  bounds, and sum their game numbers"
  [bounds games]
  (->> games
       (filter #(valid-game? bounds %))
       (map first)
       (reduce +)))

(defn total-product-maximal-hands
  "(Part 2) Find the maximal hand for each of the games (obtained by
  doing colour-wise maximum on all the hands in a game) and take the
  sum-product of the values across all games (no filtering as in part 1)"
  [games]
  (->> games
       (map second)
       (map maximal-hand)
       (map #(apply * (vals %)))
       (reduce +)))

(println (format "Problem 1: %d"
                 (total-idcs-valid-games bounds games)))

(println (format "Problem 2: %d"
                 (total-product-maximal-hands games)))
