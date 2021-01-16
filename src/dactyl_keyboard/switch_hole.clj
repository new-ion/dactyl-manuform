(ns dactyl-keyboard.switch-hole
    (:refer-clojure :exclude [use import])
    (:require 
        [scad-clj.scad :refer :all]
        [scad-clj.model :refer :all]
        [dactyl-keyboard.util :refer [plate-thickness]]
        )
    )

(def switch-height 14.4)
(def switch-width 14.4)
(def kailh-notch-thickness 1.4)

(defn single-plate [loc-plate-width]
    (let [
        top-wall (->> 
                    (cube (+ switch-width 3) loc-plate-width plate-thickness)
                    (translate [0 
                                    (+ (/ loc-plate-width 2) (/ switch-height 2))
                                    (/ plate-thickness 2)]
                        )
                    )
        left-wall (->> 
                        (cube loc-plate-width (+ switch-height 3) plate-thickness)
                        (translate [(+ (/ loc-plate-width 2) (/ switch-width 2))
                                    0
                                    (/ plate-thickness 2)]
                            )
                    )
        side-nub (->> 
                    (binding [*fn* 30] (cylinder 1 2.75))
                    (rotate (/ π 2) [1 0 0])
                    (translate [(+ (/ switch-width 2)) 0 1])
                    (hull (->> 
                                (cube base-plate-width 2.75 side-nub-thickness)
                                (translate [(+ loc-plate-width (/ switch-width 2))
                                            0
                                            (/ side-nub-thickness 2)])
                                )
                        )
                    (translate [0 0 (- plate-thickness side-nub-thickness)])
                    )
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> 
                    (cube 5 5 retention-tab-hole-thickness)
                    (translate [(+ (/ switch-width 2.5)) 
                                0 (/ retention-tab-hole-thickness 2)]
                        )
                    )
        top-nub-pair (union top-nub
                        (->> top-nub
                            (mirror [1 0 0])
                            (mirror [0 1 0])
                            )
                        )
        ]
        (difference
            (union plate-half
                (->> plate-half
                    (mirror [1 0 0])
                    (mirror [0 1 0])
                    )
                )
            (->>
                top-nub-pair
                (rotate (/ π 2) [0 0 1])
                )
            )
        )
    )


(spit "things/single-switch-hole.scad"
    (write-scad (single-plate 1.5 )))