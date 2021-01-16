(ns dactyl-keyboard.keywell
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [unicode-math.core :refer :all]
              [dactyl-keyboard.single-switch-plate :refer :all]
              ;[dactyl-keyboard.sa-keycaps :refer :all]
              ))


(def nrows 5)
(def ncols 6)                 

(def pinky-15u false)                   ; controls whether the outer column uses 1.5u keys
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column false)                ; adds an extra inner column (two less rows than nrows)
(def column-style :standard)

(if (true? inner-column) ; curvature of the columns
    (defn α [column] (cond
        (= column 0) (/ π 10) ;innermost column
        (= column 1) (/ π 10) ;inner index
        (= column 2) (/ π 10) ;index
        (= column 3) (/ π 10) ;middle
        (= column 4) (/ π 10) ;ring
        (>= column 5) (/ π 7) ;pinky
        :else (/ π 10)))
    (defn α [column] (cond
        (= column 0) (/ π 12) ;inner index
        (= column 1) (/ π 12) ;index
        (= column 2) (/ π 12) ;middle
        (= column 3) (/ π 12) ;ring
        (>= column 4) (/ π 12) ;pinky
        :else (/ π 10)))
)              
(def β (/ π 36))                        ; curvature of the rows

(if (true? inner-column) ; centerrow controls front/back tilt
    (defn centerrow [column] (cond
        (= column 0) (- nrows 3)  ;innermost column
        (= column 1) (- nrows 3)  ;inner index
        (= column 2) (- nrows 3)  ;index
        (= column 3) (- nrows 3)  ;middle
        (= column 4) (- nrows 3)  ;ring
        (>= column 5) (- nrows 3)  ;pinky
        :else (- nrows 3))
    )
    (defn centerrow [column] (cond
        (= column 0) (- nrows 3)  ;inner index
        (= column 1) (- nrows 3)  ;index
        (= column 2) (- nrows 3)  ;middle
        (= column 3) (- nrows 3)  ;ring
        (>= column 4) (- nrows 3)  ;pinky
        :else (- nrows 3) )
    )
) 


(def centercol 4)  ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 4))            ; or, change this for more precise tenting control

(if (true? inner-column)
  (defn column-offset [column] (cond
  (<= column 1) [0 -2 0]
  (= column 3) [0 2.82 -4.5]
  (>= column 5) [0 -12 5.64]            ; original [0 -5.8 5.64]
  :else [0 0 0]))
  (defn column-offset [column] (cond
  (= column 2) [0 2.82 -4.5]
  (>= column 4) [0 -5.8 5.64]            ; original [0 -5.8 5.64] - 22 Y
  :else [0 0 0]))
  )


(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 1.0)                  ; original= 0.5

(def wall-z-offset -8)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;; http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(if (true? extra-row) (def extra-cornerrow lastrow) (def extra-cornerrow cornerrow))
(if (true? inner-column) (def innercol-offset 1) (def innercol-offset 0))



(def columns (range (+ innercol-offset 0) ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(defn row-radius [column]
(+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ (α column) 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col, row]
  (if (and (true? pinky-15u) (= col lastcol) (<= row last-15u-row) (>= row first-15u-row)) 4.7625 0))
(if (true? inner-column)
  (defn pinky-ring-offset [column] (cond 
  (= column 5) 1
  :else 0))
  (defn pinky-ring-offset [column] (cond
  (= column 4) 1
  :else 0))  
)

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(+ (offset-for-column column, row) (pinky-ring-offset column) ) 0 (- (row-radius column))])
                          (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                          (translate-fn [0 0 (row-radius column)])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- (row-radius column))])
                                (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                                (translate-fn [0 0 (row-radius column)])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ (row-radius column) (nth fixed-z column)))])
                                (rotate-x-fn  (* (α column) (- (centerrow column) row)))
                                (translate-fn [0 0 (+ (row-radius column) (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
               :orthographic placed-shape-ortho
               :fixed        placed-shape-fixed
               placed-shape)
         (rotate-y-fn  tenting-angle)
         )))


(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) (true? extra-row) (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) (true? extra-row) (= ncols (+ innercol-offset 5)))
                         (and (true? inner-column) (not= row cornerrow)(= column 0))
                         (not= row lastrow))]
           (->> single-plate
                ;                (rotate (/ π 2) [0 0 1])
                (key-place column row)))))


;placement for the innermost column
(def key-holes-inner
  (if (true? inner-column)
    (apply union
           (for [row innerrows]
             (->> single-plate
                  ;               (rotate (/ π 2) [0 0 1])
                  (key-place 0 row))))))


;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; wide posts for 1.5u keys in the main cluster
(if (true? pinky-15u)
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
    (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
    (def wide-post-tl web-post-tl)
    (def wide-post-bl web-post-bl)
    (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn row-connection [column row]
  (triangle-hulls
    (key-place (inc column) row web-post-tl)
    (key-place column row web-post-tr)
    (key-place (inc column) row web-post-bl)
    (key-place column row web-post-br)))

(defn col-connection [column row]
  (triangle-hulls
    (key-place column row web-post-bl)
    (key-place column row web-post-br)
    (key-place column (inc row) web-post-tl)
    (key-place column (inc row) web-post-tr)))

(defn diag-connection [column row]
  (triangle-hulls
    (key-place column row web-post-br)
    (key-place column (inc row) web-post-tr)
    (key-place (inc column) row web-post-bl)
    (key-place (inc column) (inc row) web-post-tl)))

(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 lastrow)]
            (->> (row-connection column row)))

          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (col-connection column row))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (diag-connection column row)))))

(def inner-connectors
  (if (true? inner-column)
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0 (dec cornerrow))]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))))

(def extra-connectors
  (if (true? extra-row)
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))))

(def bottom-extra-connectors
  (union
    (triangle-hulls ; middle finger, bottom key to next up
              (key-place 1 cornerrow web-post-br)
              (key-place 2 lastrow web-post-tl)
              (key-place 2 cornerrow web-post-bl)
              (key-place 2 lastrow web-post-tr)
              (key-place 2 cornerrow web-post-br)
              (key-place 3 cornerrow web-post-bl)
    )
      
    (triangle-hulls ; ring finger, bottom key to next beside
            (key-place 3 lastrow web-post-tr)
            (key-place 3 lastrow web-post-br)
            (key-place 3 lastrow web-post-tr)
            (key-place 4 cornerrow web-post-bl)
    )  

    (row-connection 2 lastrow)
    (col-connection 3 cornerrow)
    (diag-connection 2 cornerrow)

    (triangle-hulls
      (key-place 3 cornerrow web-post-br)
      (key-place 4 cornerrow web-post-bl)
      (key-place 3 lastrow web-post-tr)

    )
    (triangle-hulls
            (key-place 1 cornerrow web-post-br)
            (key-place 2 lastrow web-post-tl)
            (key-place 2 lastrow web-post-bl)
    )
  )
)

               
; Connectors between outer column and right wall when 1.5u keys are used
(def pinky-connectors
  (if (true? pinky-15u)
    (apply union
           (concat
            ;; Row connections
            (for [row (range first-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol row wide-post-tr)
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-br))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)
               (key-place lastcol row web-post-br))))

            ;; Column connections
            (for [row (range first-15u-row last-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr)
               (key-place lastcol (inc row) wide-post-tr)))
            (if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol row wide-post-br)
               (key-place lastcol (inc row) web-post-tr))))
            (if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
              (triangle-hulls
               (key-place lastcol row web-post-br)
               (key-place lastcol (inc row) wide-post-tr)
               (key-place lastcol (inc row) web-post-tr))))
))))

(def keywell-right
    (union
        key-holes
        key-holes-inner
        connectors
        bottom-extra-connectors
        extra-connectors
        pinky-connectors
        inner-connectors
    )
)
(comment
(def caps
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 3] column)
                         (not= row lastrow))]
           (->> (sa-cap (if (and (true? pinky-15u) (= column lastcol)) 1.5 1))
                (key-place column row)))))
)

(spit "things/right-keywell.scad"
      (write-scad 
        keywell-right))

(spit "things/right-keywell-caps.scad"
  (write-scad
    (union 
      keywell-right
      ;caps
    )
  )
)