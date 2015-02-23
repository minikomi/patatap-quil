(ns patatap.shapes
  (:use [quil.core]
        [patatap.easings]))

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
    (fill 200 200 100)
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

(defrecord Veil [created lifespan leading trailing]
  Shape
  (update [{:keys [leading-easer trailing-easer created] :as this}]
    (-> this 
        (assoc :leading (leading-easer (- (millis) created)))
        (assoc :trailing (trailing-easer (- (millis) created)))))

  (draw [{:keys [leading trailing]}]
    (no-stroke)
    (fill 200 100 100)
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
        wipe-speed 800
        start (if left-to-right 0 (height))
        end   (if left-to-right (height) 0)
        ]
    (assoc (->Veil created (+ wipe-speed tail-delay) start start)
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

(defrecord Prism [created lifespan r n]
  Shape
  (update [{:keys [easer created] :as this}]
    (-> this
        (assoc :r (easer (- (millis) created)))
        ))

  (draw [{:keys [n r]}]
    (let [points (gen-prism-points n r)]

      ; webbing
      (stroke 255)
      (no-fill)
      (push-matrix)
      (translate (/ (width) 2) (/ (height) 2))
      (begin-shape)
      (doseq [[x y] points]
        (vertex x y))
      (apply vertex (first points))
      (end-shape)

      ; circles
      (no-stroke)
      (fill 255)
      (doseq [[x y] points]
        (ellipse x y (/ r 16) (/ r 16))))
    (pop-matrix))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-prism [n]
  (let [created (millis)
        min-r 10
        max-r 1000
        lifespan 1200]
    (assoc (->Prism created lifespan min-r n)
           :z-index 4
           :easer (easer-in-circ min-r max-r lifespan)
           )))

; clay
; ------------------------------------------------------------------------------

(defrecord Clay [created lifespan points center]
  Shape
  (update [{:keys [points created] :as this}]
    (-> this
        (assoc :points 
               (map #(assoc % :x ((:x-easer %) (- (millis) created))
                              :y ((:y-easer %) (- (millis) created)))
                    points))))

  (draw [{:keys [points center]}]
    (push-matrix)
    (apply translate center)
    (no-stroke)
    (fill 120 120 200)
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
            x        (* 600 (Math/cos theta))
            y        (* 600 (Math/sin theta))
            ; destination point --------------------------------------------------
            travel-theta      (- (angle-between [x y] impact) theta)
            travel-distance   (distance-between [x y] impact)
            adjusted-distance (* 2 (/ h (Math/sqrt travel-distance)))
            destination-x     (+ (* adjusted-distance (Math/cos travel-theta)) x)
            destination-y     (+ (* adjusted-distance (Math/sin travel-theta)) y)
            ]

        {:x x
         :y y
         :x-easer (easer-out-circ x destination-x lifespan)
         :y-easer (easer-out-circ y destination-y lifespan)
         }))))

(defn make-clay [n]
  (let [created (millis)
        lifespan 500
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
        points (gen-clay-points n lifespan (width) (height))
        ]
    (assoc (->Clay created lifespan points center)
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
          v-unit (/ (height) 3)
          rect-w (* 4 w-unit)
          individual-height (if (> 2 n) v-unit 
                              (* 0.8 (/ v-unit n)))
          spacing (if (> 2 n) 0
                              (/ (* 0.2 (/ v-unit n) n) (dec n)))]

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
    (assoc (->Piston created (+ wipe-speed tail-delay) color n start start)
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
      (ellipse x y 40 40))
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
           :z-index 3
           )))
