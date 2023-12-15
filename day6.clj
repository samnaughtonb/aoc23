(ns aoc23.day6)

;; Does this count as cheating?
;; The string parsing is getting a bit tedious
;; so let's just lob the data structure in here
;; as a value
(def races-part1
  '({:time 40 :dist 277}
    {:time 82 :dist 1338}
    {:time 91 :dist 1349}
    {:time 66 :dist 1063}))

(defn beats-record? [hold-time travel-time record]
  (let [distance (* hold-time travel-time)]
    (> distance record)))

(defn count-ways [race]
  (let [{time :time dist :dist} race]
    (count (filter #(beats-record? % (- time %) dist) (range 0 time)))))

(println
  (->> races-part1
       (map count-ways)
       (reduce *)))

(def races-part2
  '({:time 40829166 :dist 277133813491063}))

(println
  (count-ways (first races-part2)))
