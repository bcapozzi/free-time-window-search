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

(defn can-transition-between-at-time [w-from w-to t d-from d-to]
  (let [overlap (intersection-of w-from w-to)
        start1 (first w-from)
        end1 (last w-from)
        start2 (first w-to)
        end2 (last w-to)
        constraint1 (is-within? overlap t)
        constraint2 (> (- t start1) d-from)
        constraint3 (> (- end2 t) d-to)]
    
    ;(println constraint1, constraint2, constraint3)
    
    (and constraint1 constraint2 constraint3)))
            

    

(defn can-transition-between-at-time [w-from w-to t d-from d-to]
  (let [overlap (intersection-of w-from w-to)
        start1 (first w-from)
        end1 (last w-from)
        start2 (first w-to)
        end2 (last w-to)
        constraint1 (is-within? overlap t)
        constraint2 (> (- t start1) d-from)
        constraint3 (> (- end2 t) d-to)]
    
    ;(println constraint1, constraint2, constraint3)
    
    (and constraint1 constraint2 constraint3)))
            


    


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


