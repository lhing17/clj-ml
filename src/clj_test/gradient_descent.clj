(ns clj-test.gradient-descent
  (:require [clojure.core.matrix :refer :all]))

(defn hypothesis [theta X]
  (mmul (transpose theta) X)
  )

(defn cost [theta X y]
  "The cost function"
  (let [m (first (shape X))]
    (div (esum (square (sub (mmul X theta) y))) 2 m)
    )
  )

(defn next-theta [theta alpha X y]
  (let [m (first (shape X))]
    (sub theta
         (mul alpha (div (mmul (transpose (sub (mmul X theta) y)) X) m)))))

(defn gradient [init-theta alpha X y]
  (loop
    [theta init-theta costs [(cost theta X y)]]
    (let [n (next-theta theta alpha X y)
          c (cost n X y)
          lc (last costs)]
      (cond (> c lc) (throw (Exception. "alpha is too big."))
            (< (/ (- lc  c) lc) 0.0000000001) [theta costs]
            :else (recur n (conj costs c)))))

  )

(defn -main [& args]
  (let [init-theta [0, 0]
        X [[1 1]
           [1 2]
           [1 3]
           [1 4]
           [1 5]
           [1 6]
           [1 7]
           [1 8]
           [1 9]
           [1 10]]
        y [5.19 10.51 15.34 20.81 25.52 30.02 35.89 40.39 45.13 50.35]
        [theta costs] (gradient init-theta 0.05 X y)
        ]
    (doseq [x (map first (partition 10 (map-indexed #(vector %1 %2) costs)))]
      (println (first x) (last x))
      )
    )
  )