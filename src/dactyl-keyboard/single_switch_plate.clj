(ns dactyl-keyboard.single-switch-plate
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [unicode-math.core :refer :all]))



    (def keyswitch-height 14.15) ;14.4? original 14.15
    (def keyswitch-width 14.15)  ;14.4? original 14.15

    (def create-side-nubs? false)

    (def sa-profile-key-height 12.7)

    (def plate-thickness 4)
    (def side-nub-thickness 4)
    (def retention-tab-thickness 1.5)
    (def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
    (def mount-width (+ keyswitch-width 3.2))
    (def mount-height (+ keyswitch-height 2.7))
    (def base-plate-width 1.5)
    (if (true? innercolumn)
    (defn plate-width [column] (cond
        (= column 4) (+ base-plate-width 5)
        :else base-plate-width))
    (defn plate-width [column] (cond
        (= column 3) (+ base-plate-width 5)
        :else base-plate-width)))


    (defn single-plate [column]
    (let [top-wall (->> (cube (+ keyswitch-width 3) (base-plate-width column) plate-thickness)
                        (translate [0
                                    (+ (/ (base-plate-width column) 2) (/ keyswitch-height 2))
                                    (/ plate-thickness 2)]))
            left-wall (->> (cube plate-width (+ keyswitch-height 3) plate-thickness)
                        (translate [(+ (/ plate-width 2) (/ keyswitch-width 2))
                                    0
                                    (/ plate-thickness 2)]))
            side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                        (rotate (/ π 2) [1 0 0])
                        (translate [(+ (/ keyswitch-width 2)) 0 1])
                        (hull (->> (cube base-plate-width 2.75 side-nub-thickness)
                                    (translate [(+ (/ base-plate-width 2) (/ keyswitch-width 2))
                                                0
                                                (/ side-nub-thickness 2)])))
                        (translate [0 0 (- plate-thickness side-nub-thickness)]))
            plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
            top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                        (translate [(+ (/ keyswitch-width 2.5)) 0 (/ retention-tab-hole-thickness 2)]))
            top-nub-pair (union top-nub
                                (->> top-nub
                                    (mirror [1 0 0])
                                    (mirror [0 1 0])))]
        (difference
        (union plate-half
                (->> plate-half
                    (mirror [1 0 0])
                    (mirror [0 1 0])))
        (->>
        top-nub-pair
        (rotate (/ π 2) [0 0 1])))))



(def create-side-nubs? false)
(def keyswitch-height 14.15)  ;14.4?
(def keyswitch-width 14.15)   ; 14.4?

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))
(def base-plate-width 1.5)
(comment
(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) base-plate-width plate-thickness)
                      (translate [0
                                  (+ (/ base-plate-width 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube base-plate-width (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ base-plate-width 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube base-plate-width 2.75 side-nub-thickness)
                                 (translate [(+ (/ base-plate-width 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2.5)) 0 (/ retention-tab-hole-thickness 2)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

)

(def single-hole
    ;(single-plate 0)
    single-plate
    )

(spit "things/single-hole.scad"
      (write-scad single-hole))