(ns patatap.shapes
  (:use [quil.core]
        [patatap.easings]))


(def colors
   [
    [227 79 12]
    [163 141 116]
    [141 164 170]
    [255 197 215]
    [10 10 10]
    ]


  )

; protocol
; ------------------------------------------------------------------------------

(defprotocol Shape 
  (draw [this] "draw this shape")
  (update [this] "update this shape")
  (alive [this] "should this shape remain?"))

; util
; ------------------------------------------------------------------------------

(defn angle-between [[x1 y1] [x2 y2]]
  (Math/atan2 (- x2 x1) (- y2 y1)))

(defn distance-between [[x1 y1] [x2 y2]]
  (Math/sqrt
    (let [dx (- x2 x1)
          dy (- y2 y1)]
      (+ (* dx dx) (* dy dy)))))


; wipe
; ------------------------------------------------------------------------------

(defrecord Wipe [created lifespan leading trailing]
  Shape
  (update [{:keys [leading-easer trailing-easer created] :as this}]
    (-> this 
        (assoc :leading (leading-easer (- (millis) created)))
        (assoc :trailing (trailing-easer (- (millis) created)))))

  (draw [{:keys [leading trailing]}]
    (no-stroke)
    (apply fill (colors 0))
    (begin-shape)
    (vertex trailing 0)
    (vertex leading  0)
    (vertex leading  (height))
    (vertex trailing (height))
    (vertex trailing 0)
    (end-shape))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-wipe [left-to-right]
  (let [created (millis)
        tail-delay 450
        wipe-speed 800
        start (if left-to-right 0 (width))
        end   (if left-to-right (width) 0)
        ]
    (assoc (->Wipe created (+ wipe-speed tail-delay) start start)
           :z-index 0
           :leading-easer (easer-in-out-circ start end wipe-speed)
           :trailing-easer (->> (easer-in-out-circ start end wipe-speed) 
                                (delay-start tail-delay))
           )))


; veil
; ------------------------------------------------------------------------------

(defrecord Veil [created lifespan leading trailing color]
  Shape
  (update [{:keys [leading-easer trailing-easer created] :as this}]
    (-> this 
        (assoc :leading (leading-easer (- (millis) created)))
        (assoc :trailing (trailing-easer (- (millis) created)))))

  (draw [{:keys [leading trailing color]}]
    (no-stroke)
    (apply fill color)
    (begin-shape)
    (vertex 0       trailing)
    (vertex 0       leading)
    (vertex (width) leading)
    (vertex (width) trailing)
    (vertex 0       trailing)
    (end-shape))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-veil [left-to-right]
  (let [created (millis)
        tail-delay 450
        wipe-speed 600
        start (if left-to-right 0 (height))
        end   (if left-to-right (height) 0)
        ]
    (assoc (->Veil created (+ wipe-speed tail-delay) start start
                   (colors 3) 
                   )
           :z-index 0
           :leading-easer (easer-in-out-circ start end wipe-speed)
           :trailing-easer (->> (easer-in-out-circ start end wipe-speed) 
                                (delay-start tail-delay))
           )))

; prism
; ------------------------------------------------------------------------------

(defn gen-prism-points [n r]
  (for [i (range n)]
    (let [pct   (/ i n)
          theta (* 2 Math/PI pct)
          x     (* r (Math/cos theta))
          y     (* r (Math/sin theta))]
      [x y])))

(defrecord Prism [created lifespan r n color]
  Shape
  (update [{:keys [easer created] :as this}]
    (-> this
        (assoc :r (easer (- (millis) created)))
        ))

  (draw [{:keys [n r color]}]

    (no-fill)
    (apply stroke color)
    (stroke-weight 1)
    (let [points (gen-prism-points n r)]

      ; webbing
      (push-matrix)
      (translate (/ (width) 2) (/ (height) 2))
      (begin-shape)
      (doseq [[x y] points]
        (vertex x y))
      (apply vertex (first points))
      (end-shape)

      ; circles
      (no-stroke)
      (apply fill color)
      (doseq [[x y] points]
        (ellipse x y (/ r 24) (/ r 24))))
    (pop-matrix))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-prism [n]
  (let [created (millis)
        min-r 10
        max-r 1000
        lifespan 1200]
    (assoc (->Prism created lifespan min-r n (colors 4))
           :type :prism
           :z-index 12
           :easer (easer-in-circ min-r max-r lifespan)
           )))

; clay
; ------------------------------------------------------------------------------

(defrecord Clay [created lifespan points center color]
  Shape
  (update [{:keys [points created] :as this}]
    (-> this
        (assoc :points 
               (map #(assoc % :x ((:x-easer %) (- (millis) created))
                              :y ((:y-easer %) (- (millis) created)))
                    points))))

  (draw [{:keys [points center color]}]
    (push-matrix)
    (apply translate center)
    (apply fill color)
    (no-stroke)
    (begin-shape)
    (doseq [{:keys [x y]} points]
      (curve-vertex x y))
    (apply curve-vertex ((fn [p] [(:x p) (:y p)]) (first points))) 
    (apply curve-vertex ((fn [p] [(:x p) (:y p)]) (second points))) 
    (apply curve-vertex ((fn [p] [(:x p) (:y p)]) (nth points 2))) 
    (end-shape)
    (pop-matrix))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn gen-clay-points [n lifespan w h]
  (let [impact [(rand w) (rand h)]]
    (for [i (range n)]
      (let [; orginal point ------------------------------------------------------
            pct      (/ i n)
            theta    (* 2 Math/PI pct)
            x        (* 1200 (Math/cos theta))
            y        (* 1200 (Math/sin theta))
            ; destination point --------------------------------------------------
            travel-theta      (- (angle-between [x y] impact) theta)
            travel-distance   (distance-between [x y] impact)
            adjusted-distance (* 2 (/ h (Math/sqrt travel-distance)))
            destination-x     (+ (* adjusted-distance (Math/cos travel-theta)) x)
            destination-y     (+ (* adjusted-distance (Math/sin travel-theta)) y)
            ]
        {
         :x x
         :y y
         :x-easer (easer-out-circ x destination-x lifespan)
         :y-easer (easer-out-circ y destination-y lifespan)
         }))))

(defn make-clay [n]
  (let [created (millis)
        lifespan 600
        center (case (rand-nth [:n :nw :w :sw :s :se :e]) 
                 :n  [(/ (width) 2) 0]
                 :nw [0             0]
                 :w  [0             (/ (height) 2)]
                 :sw [0             (height)]
                 :s  [(/ (width) 2) (height)]
                 :se [(width)       (height)]
                 :e  [0             (/ (height) 2)]
                 [(width)       0]
                 )
        points (gen-clay-points n lifespan (width) (height)
                                )
        ]
    (assoc (->Clay created lifespan points center (colors 2) )
           :type :clay
           :z-index 1
           )))

; Piston
; ------------------------------------------------------------------------------

(defrecord Piston [created lifespan color n leading trailing]
  Shape
  (update [{:keys [leading-easer trailing-easer created] :as this}]
    (-> this 
        (assoc :leading (leading-easer (- (millis) created)))
        (assoc :trailing (trailing-easer (- (millis) created)))))

  (draw [{:keys [color n leading trailing]}]
    (let [w-unit (/ (width) 6)
          v-unit (/ (height) 4)
          rect-w (* 4 w-unit)
          innerheight (* v-unit 2)
          individual-height (if (> 2 n) innerheight
                              (* 0.8 (/ innerheight n)))
          spacing (if (> 2 n) 0
                              (/ (* 0.2 (/ innerheight n) n) (dec n)))]

      (push-matrix)
      (translate w-unit v-unit)

      (no-stroke)
      (apply fill color)

      (doseq [i (range n)]
        (begin-shape)

        (vertex (* trailing rect-w) (* i (+ spacing individual-height)))
        (vertex (* leading rect-w)  (* i (+ spacing individual-height)))
        (vertex (* leading rect-w)
                (+ (* i (+ spacing individual-height))
                   individual-height))
        (vertex (* trailing rect-w) 
                (+ (* i (+ spacing individual-height))
                   individual-height))
        (vertex (* trailing rect-w) (* i (+ spacing individual-height)))
        (end-shape))
      (pop-matrix))
    )

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-piston [n left-to-right]
  (let [created (millis)
        tail-delay 200
        wipe-speed 400
        start (if left-to-right 0 1)
        end   (if left-to-right 1 0)
        color [(+ 100 (* n 10)) (- 200 (* n 20)) 100] 
        ]
    (assoc (->Piston created (+ wipe-speed tail-delay) (colors (mod n 4)) n start start)
           :type :piston
           :z-index 10
           :leading-easer (easer-in-out-circ start end wipe-speed)
           :trailing-easer (->> (easer-in-out-circ start end wipe-speed) 
                                (delay-start tail-delay))
           )))


; Confetti
; ------------------------------------------------------------------------------

(defrecord Confetti [created lifespan points]
  Shape
  (update [{:keys [points created] :as this}]
    (-> this
        (assoc :points 
               (map #(assoc % :x ((:x-easer %) (- (millis) created))
                              :y ((:y-easer %) (- (millis) created)))
                    points))))

  (draw [{:keys [points center]}]
    (push-matrix)
    (no-stroke)
    (fill 220 220 200)
    (doseq [{:keys [x y]} points]
      (ellipse x y 20 20))
    (pop-matrix))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn gen-confetti-points [n lifespan orientation w h]
  (let [theta     (case orientation
                    :n  (radians 90)
                    :e  (radians 180)
                    :s  (radians 270)
                    :w  (radians 0)
                    [(/ (width) 2) (/ (height) 2)]
                    )
        deviation (radians 120)
        [center-x center-y] (case orientation
                              :n  [(/ (width) 2) 0]
                              :s  [(/ (width) 2) (height)]
                              :e  [(width)       (/ (height) 2)]
                              :w  [0             (/ (height) 2)]
                              [(/ (width) 2) (/ (height) 2)]
                              )]
    (for [i (range n)]
      (let [t (- (+ theta (* (rand) deviation)) (/ deviation 2))
            a (* (rand) 600)
            destination-x (+ center-x (* a (Math/cos t)))
            destination-y (+ center-y (* a (Math/sin t)))
            ]
        {:x center-x
         :y center-y
         :x-easer (easer-out-circ center-x destination-x lifespan)
         :y-easer (easer-out-circ center-y destination-y lifespan)
         }))))

(defn make-confetti [n]
  (let [created (millis)
        lifespan 700
        orientation (rand-nth [:n :s :e :w])
        points (gen-confetti-points n lifespan orientation (width) (height))
        ]
    (assoc (->Confetti created lifespan points)
           :type :confetti
           :z-index 3
           )))


; Glimmer
; ------------------------------------------------------------------------------

(defrecord Glimmer [created lifespan points]
  Shape
  (update [{:keys [points created] :as this}]
    (-> this
        (assoc :points 
               (map #(assoc % :line-weight ((:line-easer %) (- (millis) (% :del) created)))
                    points))))

  (draw [{:keys [points center]}]
    (no-fill)
    (doseq [{:keys [x y color r line-weight]} points]
      (apply stroke color)
      (stroke-weight (* r (Math/sin (radians line-weight))))
      (ellipse x y (- r line-weight) (- r line-weight))))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn gen-glimmer-points [n lifespan w h]
  (for [i (range n)]
    (let [theta (radians (rand-int 360))
          x (+ (/ w 2) (* (+ (* n 10) (rand-int 600)) (Math/cos theta)))
          y (+ (/ h 2) (* (+ (* n 10) (rand-int 400)) (Math/sin theta))) 
          color (rand-nth colors)
          r (+ 40 (rand-int 30))
          line-weight (+ 40 (rand-int 20))
          del (* n (rand-int 20))
          ]
      {:x x
       :y y
       :line-easer (delay-start del (easer-out-circ 0 180 (- lifespan 400 del)))
       :line-weight line-weight
       :del del
       :color color
       :r r
       })))

(defn make-glimmer [n]
  (let [created (millis)
        lifespan 1000
        points (gen-glimmer-points n lifespan (width) (height))
        ]
    (assoc (->Glimmer created lifespan points)
           :type :glimmer
           :z-index 6
           )))



; Donut
; ------------------------------------------------------------------------------

(defrecord Donut [created lifespan color w h]
  Shape
  (update [{:keys [leading-easer trailing-easer created] :as this}]
    (-> this 
        (assoc :leading (leading-easer (- (millis) created)))
        (assoc :trailing (trailing-easer (- (millis) created)))))

  (draw [{:keys [leading trailing w h angle]}]
    (no-fill)
    (stroke-weight 90)
    (apply stroke color)
    (push-matrix)
    (translate (/ w 2) (/ h 2))
    (rotate angle)
    (arc 0 0 700 700 (radians trailing) (radians leading))
    (pop-matrix)     
    )

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-donut []
  (let [created (millis)
        lifespan 1000
        tail-delay 500
        wipe-speed (- lifespan tail-delay)
        ]
    (assoc (->Donut created lifespan [225 225 225] (width) (height))
           :type :glimmer
           :z-index 5
           :angle (radians (rand-int 360))
           :leading-easer (easer-in-out-circ 0 360 wipe-speed)
           :trailing-easer (->> (easer-in-out-circ 0 360 wipe-speed) 
                                (delay-start tail-delay))
           :leading 0
           :trailing 0

           )))
