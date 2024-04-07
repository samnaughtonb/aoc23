(ns aoc23.day7
  (:require [clojure.string :as str]))

; Test Data
; (def hands
;   (list ["32T3K" 765]
;         ["T55J5" 684]
;         ["KK677" 28]
;         ["KTJJT" 220]
;         ["QQQJA" 483]))

(def hands
  ; matches structures of the test data above
  (->> "INPUT_FILE"
       System/getenv
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map #(vector (first %) (parse-long (second %))))))

(def card-order
  (list \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A))

(defn make-ordering
  "Make comparator function given ordered list of cards/characters"
  [card-order]
  (let [card-values (zipmap card-order (range (count card-order)))]
    (fn [hand1 hand2]
      (if (and (= (count hand1) 0)
               (= (count hand2) 0))
        true
        (let [x1 (-> hand1 first card-values)
              x2 (-> hand2 first card-values)]
          (if (= x1 x2)
            (recur (rest hand1) (rest hand2))
            (< x1 x2)))))))

(def hand-order             ; there are 5 cards in a hand
  (list :high-card          ; all cards labels are distinct 
        :one-pair           ; two cards same label, all others distinct
        :two-pair           ; two different pairs, third one distinct
        :three-of-a-kind    ; three cards share a label, other two different
        :full-house         ; three cards share a label, other two are a pair
        :four-of-a-kind     ; four cards same label, other is different
        :five-of-a-kind))   ; all five cards have the same label

(defn groups
  "Make data structure { card : count ... } representing a hand"
  ([hand] (groups hand {}))
  ([hand curr]
    (if (= (count hand) 0)
        curr
        (let [card (first hand)
              card-count (get-in curr [card] 0)]
          (groups (rest hand)
                  (assoc-in curr [card] (inc card-count)))))))

(defn in?
  "Helper function to check if element in collection"
  [coll elem]
  (some #(= elem %) coll))

(defn five-of-a-kind? [gps]
  (= (count gps) 1))

(defn four-of-a-kind? [gps]
  (and (= (count gps) 2)
       (in? (vals gps) 4)
       (in? (vals gps) 1)))

(defn full-house? [gps]
  (and (= (count gps) 2)
       (in? (vals gps) 3)
       (in? (vals gps) 2)))

(defn three-of-a-kind? [gps]
  (and (= (count gps) 3)
       (in? (vals gps) 3)))

(defn two-pair? [gps]
  (and (= (count gps) 3)
       (in? (vals gps) 2)
       (in? (vals gps) 1)))

(defn one-pair? [gps]
  (and (= (count gps) 4)
       (in? (vals gps) 2)))

(defn high-card? [gps]
  (= (count gps) 5))

(defn possible-groups
  "Make list of possible other groups of cards from some seed"
  [gps]
  (let [num-jacks (get-in gps [\J])]
    (if (= num-jacks 0)
        (list (dissoc gps \J))
        (let [others (for [card (filter #(not= % \J) (keys gps))]
                       (-> gps
                           (assoc-in [card] (-> card gps inc))
                           (assoc-in [\J]   (-> \J   gps dec))))]
          (mapcat possible-groups others)))))

(defn classify-by-groups [gps]
  (cond (five-of-a-kind?  gps) :five-of-a-kind 
        (four-of-a-kind?  gps) :four-of-a-kind
        (full-house?      gps) :full-house
        (three-of-a-kind? gps) :three-of-a-kind
        (two-pair?        gps) :two-pair
        (one-pair?        gps) :one-pair
        (high-card?       gps) :high-card))

(defn classify
  "Classification for Part 1 involves no transformation"
  [hand]
  (-> hand groups classify-by-groups))

(defn classify-pt2
  "Classification for Part 2 requires one to check possible other hands"
  [hand]
  (let [gps (groups hand)
        num-jacks (get-in gps [\J] 0)]
    (case num-jacks
      5 :five-of-a-kind
      0 (classify-by-groups gps)
      (let [others (possible-groups gps)
            possible-types (map classify-by-groups others)
            type-value (zipmap hand-order (range (count hand-order)))
            type-comparator (fn [t1 t2] (< (type-value t1) (type-value t2)))
            sorted (sort type-comparator possible-types)]
        (last sorted)))))

(defn classify-hands
  "Given classification function and list of hands, get a map { type: [hands] }"
  ([classify hands]
    (let [init (zipmap hand-order (repeat []))]
      (classify-hands classify hands init)))
  ([classify hands curr]
    (if (= (count hands) 0)
        curr
        (let [hand (first hands)
              class (classify (first hand))
              curr-hands (get-in curr [class])]
          (recur classify
                 (rest hands)
                 (assoc-in curr [class] (conj curr-hands hand)))))))

(defn total-winnings
  "sum(rank * bid) over all hands, noting that rank starts at one"
  [sorted-hands]
  (let [bids (map second sorted-hands)
        sum (partial reduce +)]
    (sum (for [[i bid] (zipmap (range (count bids)) bids)] (* (inc i) bid)))))

;; Part 1 solution
(let [classes (classify-hands classify hands)
      ordering (make-ordering card-order)
      comparator (fn [[h1 _] [h2 _]] (ordering h1 h2))
      sorted-classes (update-vals classes #(sort comparator %))
      flat-order (reduce concat (vals sorted-classes))]
  (println (total-winnings flat-order)))

;; Part 2 solution
(def card-order-pt2
  (list \J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A))

(let [classes (classify-hands classify-pt2 hands)
      ordering (make-ordering card-order-pt2)
      comparator (fn [[h1 _] [h2 _]] (ordering h1 h2))
      sorted-classes (update-vals classes #(sort comparator %))
      flat-order (reduce concat (vals sorted-classes))]
  (println (total-winnings flat-order)))
