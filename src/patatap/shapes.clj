(ns patatap.shapes
  (:use [quil.core]
        [patatap.easings]))

(defprotocol Shape 
  (draw [this] "draw this shape")
  (update [this] "update this shape")
  (alive [this] "should this shape remain?"))

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
           :easer (easer-in-circ min-r max-r lifespan)
           )))

; clay
; ------------------------------------------------------------------------------

(defn angle-between [[x1 y1] [x2 y2]]
  (Math/atan2 (- x2 x1) (- y2 y1)))

(defn distance-between [[x1 y1] [x2 y2]]
  (Math/sqrt
    (let [dx (- x2 x1)
          dy (- y2 y1)]
      (+ (* dx dx) (* dy dy)))))

(defn gen-clay-points [n lifespan]
  (for [i (range n)]
    (let [; orginal point ------------------------------------------------------
          pct      (/ i n)
          theta    (* 2 Math/PI pct)
          distance (* (height) (rand))
          x        (* distance (Math/sin theta))
          y        (* distance (Math/cos theta))
          ; destination point --------------------------------------------------
          impact            [(* (width) (rand)) (* (height) (rand))] 
          travel-theta      (angle-between [x y] impact)
          travel-distance   (distance-between [x y] impact)
          adjusted-distance (* 10 (/ distance (Math/sqrt travel-distance)))
          destination-x     (+ (* adjusted-distance (Math/cos travel-theta)) x)
          destination-y     (+ (* adjusted-distance (Math/cos travel-theta)) y)
          ]
      {:x x
       :y y
       :x-easer (easer-in-circ x destination-x lifespan)
       :y-easer (easer-in-circ y destination-y lifespan)
       })))

(defrecord Clay [created lifespan points orientation]
  Shape
  (update [{:keys [points] :as this}]
    (println points)
    (-> this
        (assoc :points 
               (map #(assoc % :x ((:x-easer %) (millis))
                              :y ((:y-easer %) (millis)))
                            points))))

  (draw [{:keys [points]}]
    (push-matrix)
    (translate (width) 0)
    (vertex 0 0)
    (no-stroke)
    (fill 255)
    (println points)
    (doseq [{:keys [x y]} points]
      (curve-vertex x y))
    (vertex 0 0)
    (pop-matrix))

  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created)))) 

(defn make-clay [n]
  (let [created (millis)
        lifespan 1200
        points (gen-clay-points n lifespan)
        ]
    (assoc (->Clay created lifespan points true)
           :easer (easer-in-circ 0 100 lifespan)
           )))
