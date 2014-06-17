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
    
        
