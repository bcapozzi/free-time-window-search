(ns free-time-window-search.core-spec
  (:require [speclj.core :refer :all]
            [free-time-window-search.core :refer :all]))

(describe "free-time-windows"
  (it "Can identify values within a window"
           (should= true (is-within? [1 100] 10)))

  (it "Can identify values at left edge of window as being contained within window"
           (should= true (is-within? [1 100] 1)))

  (it "Can identify values at right edge of window as being outside of window"
           (should= false (is-within? [1 100] 100)))

  (it "Can identify values to the right of window as being outside of window"
           (should= false (is-within? [1 100] 101)))

  (it "Can detect intersection of overlapping windows"
    (should= [20 100] (intersection-of [0 100] [20 10000])))

  (it "Can detect lack of intersection for windows that do not overlap"
    (should= [] (intersection-of [0 100] [101 10000])))

  (it "Can identify when a time window is reachable from another"
    (should= true (can-transition-between-at-time [0 100] [90 110] 90 2 5)))

  (it "Can identify when a time window is not reachable from another due to duration in FROM resource"
    (should= false (can-transition-between-at-time [90 100] [90 120] 95 8 10)))

  (it "Can identify when a time window is not reachable from another due to duration in TO resource"
    (should= false (can-transition-between-at-time [0 100] [90 105] 95 2 10)))

  (it "Can identify when a time window is not reachable from another due to lack of overlap"
    (should= false (can-transition-between-at-time [90 120] [100 150] 130 10 10)))

  (it "Finds single free time window spanning zero to infinity given empty loading function"
           (should= [[0 infinity]] (compute-free-time-windows [] 1)))

  (it "Can sort loading functions in terms of entry time"
           (should= [[5 10],[8 20]] (sort-loading-by-entry [[8 20],[5 10]])))

  (it "Finds free time windows given non-empty loading function"
           (should= [[0 10],[20 infinity]] (compute-free-time-windows [[10 20]] 1)))

  (it "Finds free time windows given loading function with two closed intervals"
           (should= [[0 10],[20 30],[40 infinity]] (compute-free-time-windows [[10 20] [30 40]] 1)))
  )

