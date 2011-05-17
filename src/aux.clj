(ns aux)

(defmacro if-let*
  [lets consequent subsequent]
  (loop [bindings (butlast (map vec (partition 2 lets)))
	 retval (list 'if-let (vec (reverse (take 2 (reverse lets)))) consequent subsequent)] ;; much faster than drop-last
    (if (empty? bindings) retval
	(recur (butlast bindings)
	       (list 'if-let (last bindings) retval subsequent)))))

(defmacro return-if-all
  [item & more]
  (if (seq more)
    (list 'and (cons (list 'first (conj more 'list)) (list item))
	  (conj (rest more) item 'aux/return-if-all))
    item))

(defn not-nil-or-empty? [thing]
  (not (or (nil? thing)
	   (and (coll? thing) (empty? thing)))))

(defn find-first
  [pred coll]
  (first (filter pred coll)))

(defn rcons
  "Reverse cons."
  [item coll]
  ^{:author "Kyle Harrington"}
  (reverse (cons item (reverse coll))))

(defn ensure-list
  [coll]
  (if (seq? coll) ;; will wrap vectors in lists
    coll
    (list coll)))

(defn nth-app
  [f n arg]
  (if (> n 0)
    (try (->> (f arg)
	      (recur f (dec n)))
	 (catch Exception e nil))
    arg))

(defn third
  [coll]
  (nth coll 2))