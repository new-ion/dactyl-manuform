(ns dactyl-keyboard.keywell
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [unicode-math.core :refer :all]
              [dactyl-keyboard.single-switch-plate :refer :all]
              ))

(def two-holes
    (union
        single-hole
        (->> single-hole
            (translate [10 0 0]))))


(spit "things/single-hole.scad"
      (write-scad two-holes))