(ns free-time-window-search.display
)

(import '(javax.swing JFrame JPanel )
        '(java.awt Color Graphics Graphics2D))

(defn compute-xvals [curr next time-bounds x-bounds]
  
  (let [tmin (first time-bounds)
        tmax (second time-bounds)
        xmin (first x-bounds)
        xmax (second x-bounds)
        trange (- tmax tmin)
        xrange (- xmax xmin)
        tstart (- (:entry curr) tmin)
        tend (if (nil? next) tstart (- (:entry next) tmin))
        xstart (+ xmin (* xrange (/ tstart trange)))
        xend (+ xmin (* xrange (/ tend trange)))
        ]

    [xstart xend])
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

(defn to-drawing-resources [path resources time-bounds]

  (loop [path path result []]
    (let [curr (first path)
          next (second path)
          xvals (compute-xvals curr next time-bounds [100 400])
          yvals (compute-yvals curr resources [100 400])]
      (cond
       (nil? next) result
       :else (recur (drop 1 path) (conj result {:name (:resource curr)
                                                :x1 (first xvals)
                                                :y1 (first yvals)
                                                :x2 (second xvals)
                                                :y2 (second yvals)}))
       )
      )
    
    )
  ;; [{:x1 100 :y1 100 :x2 200 :y2 100}
  ;;  {:x1 200 :y1 200 :x2 300 :y2 200}]
  )

(defn average [x1 x2]
  (/ (+ x1 x2) 2)
  )

(defn render-path [g w h path resources time-bounds]
  (let [dresources (to-drawing-resources path resources time-bounds)]
    (.setColor g (new Color 0 255 0 128))
    (doall (for [d dresources]
             (.fillRect g (:x1 d) (:y1 d) (- (:x2 d) (:x1 d)) (- (:y2 d) (:y1 d)))
             ))
    )
  )

(defn find-distinct-resources [paths]

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

(defn find-time-bounds [paths]
  [0 20]
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
  ;; (.drawString g "R" 20 200)
   (let [label-y (compute-resource-label-y id resources y-bounds)]
     (.setColor g (new Color 0 255 0 255))
     (.drawString g id x-posn label-y)
     )
  )

(defn render-timeline [g w h paths]
  
  (let [resources (find-distinct-resources paths)
        time-bounds (find-time-bounds paths)]

     (doall
      (for [r resources]
        (render-resource-label g r resources 20 [100 400])))  

     (.drawLine g 100 50 400 50)
     (.drawString g "time" 410 54)

     )
 )

(defn render-paths [g w h paths]
  (let [resources (find-distinct-resources paths)
        time-bounds (find-time-bounds paths)]

    (loop [paths paths]
      (if (> (count paths) 0)
        (let [curr (:path (first paths))]
          (render-path g w h curr resources time-bounds)
          (recur (drop 1 paths))))
      )

    )
  )

(defn get-paths []

  [{:user "F1" :path [{:resource "A" :entry 0}
                      {:resource "B" :entry 10}
                      {:resource "C" :entry 20}]}]

  )

(defn render [ #^Graphics g w h ]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h)
    (.setColor (Color/GREEN))
    ;; (.drawArc 200 200 20 20 0 360)
    )


  (render-paths g w h (get-paths))
  (render-timeline g w h (get-paths))  
  )

(defn create-panel []
    "Create a panel with a customised render"

  (proxy [JPanel] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (render g (. this getWidth) (. this getHeight)))))


(defn run []

  (let [frame (JFrame. "Free Time Window Search Visualization")
        panel (create-panel)]

    (doto frame
      (.add panel)
      (.setSize 640 400)
      (.setVisible true))


    (doto panel
      (.repaint)))
  )


