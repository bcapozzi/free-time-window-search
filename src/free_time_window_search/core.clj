(ns free-time-window-search.core) 

(def ^:const infinity 999999)

(defn sort-loading-by-entry [loading] 
  (sort-by first loading)
)

(defn compute-free-time-windows [loading capacity]
  (let [loading-func (sort-loading-by-entry loading)]
    (cond
      (= 0 (count loading)) [0 infinity]
      :else []))
  )

(defn is-within? [window t]
  "Checks to see if a value is within a window closed on the left, open on the right: [a,b)"
  (and (>= t (first window)) (< t (last window))))

(def ^:const infinity 999999)

(defn sort-loading-by-entry [loading] 
  (sort-by first loading)
)


(defn is-within? [window t]
  "Checks to see if a value is within a window closed on the left, open on the right: [a,b)"
  (and (>= t (first window)) (< t (last window))))
 
(defn intersection-of
  "Compute the intersection of two ranges, each defined by two integer values [a1,b1] and [a2,b2]"
  [range1 range2]
  (let [a1 (first range1)
    b1 (last range1)
    a2 (first range2)
    b2 (last range2)
    start (max a1 a2)
    end (min b1 b2)]
(cond 
  (< start end) [start end]
  :else [])))


(defn can-transition-between-at-time? [from to t]
  (let [overlap (intersection-of (:window from) (:window to))
        start1 (first (:window from))
        end1 (last (:window from))
        start2 (first (:window to))
        end2 (last (:window to))
        constraint1 (is-within? overlap t)
        constraint2 (>= (- t start1) (:duration from))
        constraint3 (>= (- end2 t) (:duration to))]
    
    ;; (println "time: ", t, " overlap: " constraint1, constraint2, constraint3)
    
    (and constraint1 constraint2 constraint3)))
            

(defn can-transition-between-given-entry-time? [from to entry-time]
  (loop [t (+ (:entry from) (:duration from))]
    (cond
     (> t (second (:window to))) false
     (can-transition-between-at-time? from to t) true
     :else (recur (inc t)))
    )
  )

(defn to-changes [load-func]
  (loop [lfunc load-func
         changes [[0 0]]]
;    (println lfunc, changes)
    (cond
      (= 0 (count lfunc)) changes
      :else (recur (drop 1 lfunc) (conj changes [(first (first lfunc)) 1] [(last (first lfunc)) -1]))))) 
;                   (conj changes [(first (first lfunc)) 1 (last (first lfunc)) -1])))))

(defn is-still-open? [usage capacity]
  (< usage capacity)
  )

(defn update-usage [usage change]
  (cond
    (nil? change) usage 
    (empty? change) usage 
    :else (+ usage (last change))))

(defn get-time [change]
  (cond
    (nil? change) 0
    :else (first change)))


(defn to-free-windows [changes capacity]
  (loop [start 0 is_open true usage 0 changes-left changes free-windows []]
    (let [curr-change (first changes-left)
          u (update-usage usage curr-change)
          t (get-time curr-change)]
     (cond
        (empty? changes-left) (conj free-windows [start 999999])
        (and is_open (>= u capacity)) (recur -1 false u (drop 1 changes-left) (conj free-windows [start (first curr-change)])) 
        (and (not is_open) (< u capacity)) (recur t true u (drop 1 changes-left) free-windows)
        :else (recur start is_open u (drop 1 changes-left) free-windows) 
       ))))

(defn compute-free-time-windows [loading capacity]
  (let [loading-func (sort-loading-by-entry loading)
        changes (to-changes loading-func)]
      (to-free-windows changes capacity)
    )
  )

(defn compute-exit [usage]
  (merge  {:exit  (+ (:entry usage) (:duration usage))} usage )
)


(defn remove-lowest-cost [open-list]
  (let [best (first (sort-by :exit (map compute-exit open-list)))]
    [best (rest open-list)]
    )
  )

(defn is-neighbor? [usage resource]
  (contains? (set (:neighbors usage)) (:resource resource)))

(defn get-neighbors-of [usage resources]
  (filter #(is-neighbor? usage %) resources)
  )

(defn find-reachable-between [from-resource-usage next-resource]
  (let [free-windows (compute-free-time-windows (:loading next-resource) (:capacity next-resource))]
    (loop [free-windows free-windows reachable []]
       ;; (println free-windows, reachable)
      (cond
       (empty? free-windows) reachable
       (can-transition-between-given-entry-time?
        from-resource-usage
        (merge  {:window (first free-windows)} next-resource)
        (:entry from-resource-usage))
       (recur (drop 1 free-windows) (conj reachable {:resource (:resource next-resource):window (first free-windows)})))
      )
    )
  )


(defn find-reachable-windows [from-resource-usage resources]
  (let [neighbors (get-neighbors-of from-resource-usage resources)]
    (loop [neighbors neighbors reachable []]
      (cond
       (empty? neighbors) reachable 
       :else (recur (drop 1 neighbors) (conj reachable (find-reachable-between from-resource-usage (first neighbors)))))
      )
    )
  )
