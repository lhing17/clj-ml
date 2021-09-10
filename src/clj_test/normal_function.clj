(ns clj-test.normal-function
  (:require [clojure.core.matrix :refer :all]))

(defn normal-function [X y]
  (mmul (inverse (mmul (transpose X) X)) (transpose X) y)
  )

(defn -main [& args]
  (println (normal-function [[1 1]
                             [1 2]
                             [1 3]
                             [1 4]
                             [1 5]
                             [1 6]
                             [1 7]
                             [1 8]
                             [1 9]
                             [1 10]]
                            [5.19
                             10.51
                             15.34
                             20.81
                             25.52
                             30.02
                             35.89
                             40.39
                             45.13
                             50.35
                             ]

                            ))
  )
