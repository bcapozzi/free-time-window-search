(ns free-time-window-search.display-spec
  (:require [speclj.core :refer :all]
            [free-time-window-search.display :refer :all]))

(describe "display"
          (let
              [x-bounds [100 400]
               t-bounds [0 30]]
            (it "can convert time values to display units"
                (should= 100 (to-display-x x-bounds t-bounds 0))
                (should= 400 (to-display-x x-bounds t-bounds 30))
                (should= 250 (to-display-x x-bounds t-bounds 15))
                )
            )

          (let
              [resources ["A" "B" "C"]
               y-bounds [100 400]]
            (it "can convert resource id to display units"
                (should= 100
                         (first (compute-yvals {:resource "A"}
                                                resources
                                                y-bounds)))
                (should= 200
                         (first (compute-yvals {:resource "B"}
                                                resources
                                                y-bounds)))
                )
            )

          
          )
