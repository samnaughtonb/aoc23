# Solutions for Advent of Code 2023

## Day 1

Given that the first part is so trivial, just converting digit characters
to integers and summing them, I ended up with something like this.

```clojure
(defn digit? [char]
  (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} char))

(defn calibration-value [input]
  (let [digits (->> input
                    (filter digit?)
                    (map #(- (int %) 48)))]
    (+ (* 10 (first digits)) (last digits))))
```

This is far from optimal for purposes of the additional functionality
required by the second part (i.e. checking for instances of 'one' and
other word-representations of numbers). I threw out this code and replaced
it with something more generic. 

### Day 2

Perhaps I'm just getting into it, but I found Day 2 much easier than Day 1
in Clojure. The `clojure.string.split` function is very useful for parsing
the input data, but once you have a data structure in the Clojure world
representing the salient information, the logic is very trivial to write
(see the `maximal-hand` and `valid-bag?` functions.)

### Day 4

Sod doing docstrings (already!), slightly fiddlier to build up the data
structure representing the totals for part 2 but came out quite nice in
the end.

### Day 6

Surprisingly easy logic today compared to the previous days - although I
imagine the solution would have been decently longer if I'd done the table
parsing properly.
