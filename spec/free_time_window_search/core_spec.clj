
(ns free-time-window-search.core-spec
  (:require [speclj.core :refer :all]
            [free-time-window-search.core :refer :all]))


(describe "free-time-windows"
  (it "Can identify values within a window"
           (should= true (is-within? [1 100] 10)))

  (it "Can identify values at
left edge of window as being contained within window"
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
      (should= true (can-transition-between-at-time? {:window [0 100] :duration 2}
                                                     {:window [90 110] :duration 5} 95)))

   
  (it "Can identify when a time window is not reachable from another due to duration in FROM resource"
      (should= false (can-transition-between-at-time? {:window [90 100] :duration 8}
                                                       {:window [90 120] :duration 10} 95)))

  (it "Can identify when a time window is not reachable from another due to duration in TO resource"
      (should= false (can-transition-between-at-time? {:window [0 100] :duration 2}
                                                      {:window [90 105] :duration 11} 95)))

  (it "Can identify when a time window is not reachable from another due to lack of overlap"
      (should= false (can-transition-between-at-time? {:window [90 120] :duration 10}
                                                      {:window [100 150] :duration 10} 130)))

  (it "Finds single free time window spanning zero to infinity given empty loading function"
      (should= [[0 infinity]] (compute-free-time-windows [] 1)))

  (it "Can sort loading functions in terms of entry time"
      (should= [[5 10],[8 20]] (sort-loading-by-entry [[8 20],[5 10]])))

  (it "Finds free time windows given non-empty loading function"
      (should= [[0 10],[20 infinity]] (compute-free-time-windows [[10 20]] 1)))

  (it "Finds free time windows given loading function with two closed intervals"
      (should= [[0 10],[20 30],[40 infinity]] (compute-free-time-windows [[10 20] [30 40]] 1)))

  (it "Can select element from open list with earliest exit time, given durations on each resource"
      (should= "A"
               (:resource (first  (remove-lowest-cost [{:resource "B" :entry 5 :duration 15}
                                                       {:resource "C" :entry 12 :duration 20}
                                                       {:resource "A" :entry 0 :duration 10}])))))

  (it "Removes lowest cost element from open list, reducing size by one"
      (should= 2
               (count (second (remove-lowest-cost [{:resource "B" :entry 5 :duration 15}
                                                       {:resource "C" :entry 12 :duration 20}
                                                       {:resource "A" :entry 0 :duration 10}])))))

  (let
      [from-resource {:resource "A" :window [0 99999] :entry 0 :duration 1 :neighbors ["B"]}
       all-resources [{:resource "A" :loading [] :capacity 99999 :duration 0}
                      {:resource "B" :loading [[2 4]] :capacity 1 :duration 1}]]
    (it "Can identify reachable neighbors"
        (should-contain [0 2]
                        (map :window (first (find-reachable-windows from-resource all-resources))))
        (should-contain [4 infinity]
                        (map :window (first (find-reachable-windows from-resource all-resources))))
    ))
  
  )


