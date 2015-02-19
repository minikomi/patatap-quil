(ns patatap.shapes
  (:use [quil.core]
        [patatap.easings]))

(defprotocol Shape 
  (draw [this] "draw this shape")
  (update [this] "update this shape")
  (alive [this] "should this shape remain?"))

; wipe
; ------------------------------------------------------------------------------

(defrecord Wipe [created lifespan]
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
    (vertex leading 0)
    (vertex leading (height))
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
    (assoc (->Wipe created (+ wipe-speed tail-delay))
           :leading start
           :trailing start
           :leading-easer (easer-circ start end wipe-speed)
           :trailing-easer (->> (easer-circ start end wipe-speed) 
                                (delay-start tail-delay))
           )))


; circle
; ------------------------------------------------------------------------------

(defrecord Circle [created r lifespan]
  Shape
  (update [{:keys [created easeradius] :as this}]
    (-> this 
        (assoc :r (easeradius (- (millis) created)))
        ))
  (draw [{:keys [r created]}]
    (apply fill [100 100 20])
    (ellipse (+ r 100) (+ 100 (/ (- (millis) created) 10)) 12 12))
  (alive [{:keys [lifespan created]}]
    (>= lifespan (- (millis) created))))

(defn make-circle [circ]
  (let [lifespan 4000
        start    0
        end      600]
    (assoc (->Circle (millis) start lifespan)
           :easeradius (if circ (easer-circ start end lifespan)
                                (easer-quint start end lifespan)
                         ))))

