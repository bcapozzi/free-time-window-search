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
  ;; (println "checking for intersection between: ", range1, " and ", range2)
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
  ;; (println "checking for transition between " from " and " to " at time: " t)
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
  ;; (println "computing exit for usage: ", usage)
  (merge  {:exit  (+ (:entry usage) (:duration usage))} usage )
)

(defn select-lowest-cost [open-list]
  ;; (println "searching for lowest cost on open-list: ", open-list)
  (first (sort-by :exit (map compute-exit open-list)))  
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
  
  (let [free-windows (compute-free-time-windows (:loading next-resource)
                                                (:capacity next-resource))]
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
       :else (recur (drop 1 neighbors)
                    (conj reachable
                          (find-reachable-between from-resource-usage (first neighbors)))))
      )
    )
  )

(defn add-source [from start resources]
  ;; (println "adding source node to resources: ", resources)
  (into [ {:resource "source"  :entry start :duration 0 :neighbors [from]}] resources)
  )

(defn is-destination? [goal node]
  (= goal (:resource node))
  )

(defn matches-id? [resource id]
  (= id (:resource resource))
  )

(defn get-resource [resources id]
  (first (filter #(matches-id? % id) resources))
  )

(defn trace-path [node resources]
  ;; (println "found destination!! ", node)
  (loop [path [] node node]
    ;; (println "path so far: " path " node: " (:resource  node) )
    (cond
     (empty? (:previous node)) path
     :else (recur (into [{:resource  (:resource node)
                          :entry (:entry node)}] path) (:previous node))
     )
    )
  )

(defn get-resource-ids [resource-path]
  (map :resource resource-path)
  )

(defn get-resource-entry-times [resource-path]
  (map :entry resource-path)
  )

(defn expand [open-list node resources]
  ;; (println "expanding node: " node)
  (let [open-list (rest open-list)
        reachable (find-reachable-windows node resources)]
    (loop [reachable reachable open-list open-list]
      ;; (println "reachable windows: ", reachable)
      ;; (println "first reachable: ", (first reachable))
      (cond
       (empty? reachable) open-list
       :else
       (let [named-window (first (first reachable))
             texit (+ (:entry node) (:duration node))
             tentry (max texit (first (:window named-window)))
             resource-id (:resource named-window)
             resource (get-resource resources resource-id)]
         (recur (drop 1 reachable)
                (conj open-list
                      (assoc
                          (assoc
                              (assoc resource :entry tentry)
                            :window (:window named-window))
                        :previous node)))
         )
       )
      )
    )
  )

(defn find-path [from to start resources]
  (let [resources (add-source from start resources)]
    ;; (println "search for path from ", from, " to ", to, " on resources: ", resources)
    (loop [open-list [(assoc  (first resources) :window [0 infinity])]]
      (let [next (select-lowest-cost open-list)]
        (cond
         (empty? open-list) []
         (is-destination? to next) (trace-path next resources)
         :else (recur (expand open-list next resources))
         ))))
  )
