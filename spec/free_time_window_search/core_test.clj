(ns free-time-window-search.core-test
  (:require [clojure.test :refer :all]
            [free-time-window-search.core :refer :all]))

(deftest window-tests
  
  (testing "Can identify values within a window"
           (is (= true (is-within? [1 100] 10))))
  
  (testing "Can identify values at left edge of window as being contained within window"
           (is (= true (is-within? [1 100] 1))))

  (testing "Can identify values at right edge of window as being outside of window"
           (is (= false (is-within? [1 100] 100))))
  
  (testing "Can identify values to the right of window as being outside of window"
           (is (= false (is-within? [1 100] 101))))
  
  (testing "Can detect intersection of overlapping windows"
    (is (= [20 100] (intersection-of [0 100] [20 10000]))))
  
  (testing "Can detect lack of intersection for windows that do not overlap"
    (is (= [] (intersection-of [0 100] [101 10000]))))
  
  (testing "Can identify when a time window is reachable from another"
    (is (= true (can-transition-between-at-time [0 100] [90 110] 90 2 5))))
  
  (testing "Can identify when a time window is not reachable from another due to duration in FROM resource"
    (is (= false (can-transition-between-at-time [90 100] [90 120] 95 8 10))))
  
  (testing "Can identify when a time window is not reachable from another due to duration in TO resource"
    (is (= false (can-transition-between-at-time [0 100] [90 105] 95 2 10))))
  
  (testing "Can identify when a time window is not reachable from another due to lack of overlap"
    (is (= false (can-transition-between-at-time [90 120] [100 150] 130 10 10))))
  
  (testing "Finds single free time window spanning zero to infinity given empty loading function"
           (is (= [0 infinity] (compute-free-time-windows [] 1))))
           
  (testing "Can sort loading functions in terms of entry time"
           (is (= [[5 10],[8 20]] (sort-loading-by-entry [[8 20],[5 10]]))))
  
  (testing "Finds free time windows given non-empty loading function"
           (is (= [[0 10],[21 infinity]] (compute-free-time-windows [[10 20]] 1))))
  
  )
