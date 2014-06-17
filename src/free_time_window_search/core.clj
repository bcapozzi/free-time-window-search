(ns free-time-window-search.core) 

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



