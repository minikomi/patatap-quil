(ns patatap.ui
  (:use [quil.core]))

(defprotocol UIElement
  (draw [this] "draw this ui element")
  (event-within?  [this event] "was the click within this element?")
  (handle-mousedown   [this event] "handle mouse down")
  (handle-mouseup [this event] "handle mouse up")
  (handle-mousemove   [this event] "handle mouse drag"))

(defn within? [x y w h ev-x ev-y]
  (and (<= x ev-x)
       (<= y ev-y)
       (> (+ x w) ev-x)
       (> (+ y h) ev-y)))

; Toggle Box
; ------------------------------------------------------------------------------

(defrecord ToggleBox [x y title active]
  UIElement
  (draw [this]
    (push-matrix)
    (translate x y)
    (no-stroke)
    (apply fill (if active [180 50 50] [60 60 180]))
    (rect 0 0 14 14)
    (fill 220 10 220)
    (text-size 16)
    (text title 18 12)
    (pop-matrix))
  (handle-mousedown [this {ev-x :x ev-y :y}]
    (if (within? x y 20 20 ev-x ev-y)
      (update-in this [:active] not)
      this))
  (handle-mouseup [this event] this)
  (handle-mousemove    [this event] this))

; Slider
; ------------------------------------------------------------------------------

(defrecord Slider [x y width title min-v max-v current]
  UIElement
  (draw [{:keys [active] :as this}]
    (push-matrix)
    (translate x y)
    (no-stroke)
    ; background
    (apply fill [90 90 90 120])
    (rect 0 0 width 14)
    ; center
    (apply fill (if active [180 50 50] [60 60 180]))
    (let [value-width (* width (/ (- current min-v) (- max-v min-v)))] 
      (rect 0 0 value-width 14))
    ; title
    (fill 220 10 220)
    (text-size 16)
    (text title (+ width 2) 12)
    ; text
    (fill 255)
    (text-size 16)
    (text (str current) 2 14)
    (pop-matrix))

  (handle-mousedown [this {ev-x :x ev-y :y}]
    (if (within? x y width 20 ev-x ev-y)
      (assoc this 
             :active true
             :current
             (cond (> x ev-x) min-v
                   (< (+ x width) ev-x) max-v
                   :else (Math/floor
                           (+ min-v 
                            (* (- max-v min-v)
                               (/ (- ev-x x) width))))))
      this
      ))

  (handle-mouseup [this event] 
    (assoc this :active false))

  (handle-mousemove [this {ev-x :x}]
    (if (:active this)
      (assoc this 
             :current
             (cond (> x ev-x) min-v
                   (< (+ x width) ev-x) max-v
                   :else (Math/floor 
                           (+ min-v 
                              (* (- max-v min-v)
                                 (/ (- ev-x x) width))))))
      this
      )))

