(ns dactyl-keyboard.kailh-hole
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer [plate-thickness]]))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.4) ;; Was 14.1, then 14.25
(def keyswitch-width 14.4)
(def kailh-notch-thickness 1.4) ;; Distance from top of plate to teeth

(def kailh-hole
  (let [top-wall (->> (difference
                       (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                       (->>
                        (cube (/ keyswitch-width 3) 1.6 plate-thickness)
                        (translate [0
                                    0
                                    (* -1 kailh-notch-thickness)])))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        plate-half (union top-wall left-wall)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

;;;;;;;;;;;;;;;;;
;; Write Files ;;
;;;;;;;;;;;;;;;;;

(spit "things/kailh-hole.scad"
      (write-scad kailh-hole))
