(ns linear)

(defn v+ [& vectors]
  (apply mapv + vectors))

(defn v- [vector1 & vectors]
  (apply mapv - vector1 vectors))

(defn v* [& vectors]
  (apply mapv * vectors))

(defn vd [vector1 & vectors]
  (apply mapv / vector1 vectors))

(defn dot [& vectors]
  (if (empty? vectors) 0 (apply + (apply mapv * vectors))))

(defn v*s [vector & scalars]
  (let [scalarProduct (apply * scalars)] (mapv #(* % scalarProduct) vector)))

(defn m+ [& matrices]
  (apply mapv v+ matrices))

(defn m- [matrix1 & matrices]
  (apply mapv v- matrix1 matrices))

(defn m* [& matrices]
  (apply mapv v* matrices))

(defn md [matrix1 & matrices]
  (apply mapv vd matrix1 matrices))

(defn m*s [matrix & scalars]
  (let [scalarProduct (apply * scalars)] (mapv #(v*s % scalarProduct) matrix)))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*v [matrix vector]
  (mapv #(dot % vector) matrix))

(defn m*m [& matrices]
  (reduce
    (fn [result matrix] (mapv #(mapv (partial dot %) (transpose matrix)) result)) matrices))
