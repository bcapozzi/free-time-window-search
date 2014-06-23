(ns free-time-window-search.display)

(import '(javax.swing JFrame JPanel )
        '(java.awt Color Graphics Graphics2D))

;; default grid-line setting
;; is overwritten by data contents
;; can be manually set as well
(def grid-lines (ref [10 20 30]))

(def drawing-panel-width 640)
(def drawing-panel-height 480)
(def drawing-resource-label-x 20)

(def grid-line-color (new Color 64 64 64 255))
(def resource-label-color (new Color 255 255 255 255))

(def user-color-map (ref {}))

(defn update-grid-lines [vals panel]
  (sync nil
        (ref-set grid-lines vals))

  (if (not (nil? panel))
    (.repaint panel))
  )

(defn compute-xvals
                     [curr time-bounds x-bounds]

  (cond
   (nil? curr) []
   :else
   (let [tmin (first time-bounds)
        tmax (second time-bounds)
        xmin (first x-bounds)
        xmax (second x-bounds)
        trange (- tmax tmin)
        xrange (- xmax xmin)
        tstart (- (:entry curr) tmin)
        tend (- (+ (:entry curr) (:duration curr)) tmin)
        xstart (+ xmin (* xrange (/ tstart trange)))
        xend (+ xmin (* xrange (/ tend trange)))
        ]

    [xstart xend]))
  
  )


(defn compute-yvals [curr resources y-bounds]
  (let [index (.indexOf resources (:resource curr))
        n (count resources)
        ymin (first y-bounds)
        ymax (second y-bounds)
        yrange (- ymax ymin)
        yval (+ ymin (* yrange (/ index n)))
        h (* (/ 1 n) yrange)
        ]
    [yval (+ yval (* 0.95 h))]
    )
  )

(defn to-drawing-resources [path resources time-bounds x-bounds y-bounds]
  (loop [path path result []]
    (let [curr (first path)
          xvals (compute-xvals curr time-bounds x-bounds)
          yvals (compute-yvals curr resources y-bounds)]
      (cond
       (empty? path) result
       :else
       (recur (drop 1 path) (conj result {:name (:resource curr)
                                          :x1 (first xvals)
                                          :y1 (first yvals)
                                          :x2 (second xvals)
                                          :y2 (second yvals)})))
      )
    )
  )

(defn average [x1 x2]
  (/ (+ x1 x2) 2)
  )

(defn render-resource-usage 
  "Display time spent in resource as rectangle drawn in a given row of the chart.
  Note that resources with zero time are drawn as circles"
  [g color usage user]
  (let [x1 (:x1 usage)
        y1 (:y1 usage)
        x2 (:x2 usage)
        y2 (:y2 usage)
        dx (- x2 x1)]

    (.setColor g color)

    (if (= 0 dx)
      (.fillArc g (- (int (average x1 x2)) 5) (- (int (average y1 y2)) 5) 10 10 0 360)
      (.fillRect g
             (:x1 usage)
             (:y1 usage)
             (- (:x2 usage) (:x1 usage))
             (- (:y2 usage) (:y1 usage)))
      )

    (.setColor g resource-label-color)
    (.drawString g
                user
                (int  (average (:x1 usage) (:x2 usage)))
                (int  (average (:y1 usage) (:y2 usage))))

    )
  )

;; leave x% of pixels on each side for labels
(defn compute-drawing-x-bounds [w]
  (let [margin (int (* 0.1 w))]
    [margin (- w margin)]
    ))

;; leave x% of pixels on each side for labels
(defn compute-drawing-y-bounds [h]
    (let [margin (int (* 0.1 h))]
    [margin (- h margin)]
    ))

(defn render-path
  "Render resource usage as a function of time, like a gantt chart"
  [g w h color user path resources time-bounds]
  (let [x-bounds (compute-drawing-x-bounds w)
        y-bounds (compute-drawing-y-bounds h)
        dresources (to-drawing-resources path resources time-bounds x-bounds y-bounds)]
    
    (doall (for [d dresources]
             (render-resource-usage g color d user)
             ))
    )
  )

(defn find-distinct-resources
  "Search over paths to find unique resources used"
  [paths]
  (loop [paths paths uresources []]
    (let [path (:path  (first paths))
          ids (vec (map :resource path))]
      (println path ids uresources)
      (cond
       (empty? paths) (vec (distinct  uresources))
       :else (recur (drop 1 paths) (apply conj uresources ids)))
      )
    )
  )

(defn compute-exit-time [usage]
  (+ (:entry usage) (:duration usage))
  )

(defn find-earliest-time [user-path]
  (let [path (:path user-path)
        entries (map :entry path)]
    (apply min entries)
    )
  )

(defn find-latest-time [user-path]
  (let [path (:path user-path)
        entries (map compute-exit-time path)]
    (apply max entries)
    )
  )

(defn find-time-bounds [paths]
  (let [max-vals (for [path paths](find-latest-time path))
        min-vals (for [path paths](find-earliest-time path))]
    [(apply min min-vals)  (apply max max-vals)]
    )
)

(defn compute-resource-label-y [id resources y-bounds]
  (let [index (.indexOf resources id)
        ymin (first y-bounds)
        ymax (second y-bounds)
        yrange (- ymax ymin)
        n (count resources)
        yval (+ ymin (* yrange (/ index n)))
        h (* (/ 1 n) yrange)
        ydraw (average yval (+ yval h))
        ]
    (int  ydraw)
    ))
  
(defn render-resource-label [g id resources x-posn y-bounds]
   (let [label-y (compute-resource-label-y id resources y-bounds)]
     (.setColor g resource-label-color)
     (.drawString g id x-posn label-y)
     )
  )

(defn to-display-x [x-bounds t-bounds t]
  (let [x-min (first x-bounds)
        x-max (second x-bounds)
        t-min (first t-bounds)
        t-max (second t-bounds)
        x-range (- x-max x-min)
        t-range (- t-max t-min)
        slope (/ x-range t-range)]

    (int (+ x-min (* slope (- t t-min))))
    )
  )

(defn render-time-grid-line [g t time-bounds x-bounds min-y max-y]
  (let [x (to-display-x x-bounds time-bounds t)]
    (.setColor g grid-line-color)
    (.drawLine g x min-y x max-y)
    (.drawString g (str  t) (-  x 4) (- min-y 4))
    )
  )

(defn compute-min-timeline-y [y-bounds]
  (let [ymin (first y-bounds)
        ymax (second y-bounds)
        margin (int (* 0.1 (- ymax ymin)))]
    margin)
  )

(defn compute-max-timeline-y [y-bounds]
  (let [ymin (first y-bounds)
        ymax (second y-bounds)
        margin (- ymax  (int (* 0.02 (- ymax ymin))))]
    margin)
  )

(defn compute-time-grid-values [time-bounds]
  (take-nth 10 (range (first time-bounds)
                      (second time-bounds))))

(defn render-timeline [g w h paths]
  
  (let [resources (find-distinct-resources paths)
        time-bounds (find-time-bounds paths)
        time-grid-vals (compute-time-grid-values time-bounds)
        x-bounds (compute-drawing-x-bounds w)
        y-bounds (compute-drawing-y-bounds h)
        min-timeline-y (compute-min-timeline-y y-bounds)
        max-timeline-y (compute-max-timeline-y y-bounds)]
    
    (doall
     (for [r resources]
       (render-resource-label g r resources drawing-resource-label-x y-bounds)))  

    (.setColor g grid-line-color)
    (.drawLine g
               (first x-bounds)
               min-timeline-y
               (second x-bounds)
               min-timeline-y)
    
    (.drawString g "time" (+ (second x-bounds) 10) (+ min-timeline-y 4))

    ;; pass nil in for panel
    (update-grid-lines time-grid-vals nil)
    (doall
     (for [t @grid-lines]
       (render-time-grid-line g
                              t
                              time-bounds
                              x-bounds
                              min-timeline-y
                              max-timeline-y)
       )
     ))
  )


(defn assign-user-color
  "Assign a color for each user; for now a random point in (r,g,b)"
  [id]
  (let [r (rand-int 255)
        g (rand-int 255)
        b (rand-int 255)
        c (new Color r g b 128)]

    (sync nil
          (alter user-color-map assoc id c)
          )
    c
    )
  )

(defn get-color [user-id]
  (let [c (get @user-color-map user-id)]
    (cond
     (nil? c) (assign-user-color user-id)
     :else c)
    )
  )
       
(defn render-paths [g w h paths]
  (let [resources (find-distinct-resources paths)
        time-bounds (find-time-bounds paths)]

    (loop [paths paths]
      (if (> (count paths) 0)
        (let [curr (:path (first paths))
              user (:user (first paths))]
          (render-path g w h (get-color user) user curr resources time-bounds)
          (recur (drop 1 paths))))
      )

    )
  )

(defn get-paths
  "This is the test data that will be rendered, representative output of search"
  []

  [{:user "F1" :path [{:resource "A" :entry 0 :duration 10}
                      {:resource "B" :entry 10 :duration 10}
                      {:resource "C" :entry 20 :duration 5}]}
   {:user "F2" :path [{:resource "A" :entry 10 :duration 10}
                      {:resource "B" :entry 20 :duration 10}
                      {:resource "C" :entry 30 :duration 5}]}
   {:user "F3" :path [{:resource "C" :entry 35 :duration 7}
                      {:resource "B" :entry 42 :duration 12}
                      {:resource "A" :entry 54 :duration 9}]}]

  )

(defn render [ g w h user-paths]
  (doto g
      (.setColor (Color/BLACK))
      (.fillRect 0 0 w h)
      )

    (render-timeline g w h user-paths)  
    (render-paths g w h user-paths)
  )

(defn create-panel [user-paths]

  (proxy [JPanel] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (render g (. this getWidth) (. this getHeight) user-paths))))


(defn run [user-paths]

  (let [frame (JFrame. "Free Time Window Search Visualization")
        panel (create-panel user-paths)]

    (doto frame
      (.add panel)
      (.setSize drawing-panel-width drawing-panel-height)
      (.setVisible true))

    (doto panel
      (.repaint))

    ;; return the panel so can manually tweak various aspects 
    panel
    )
  )



