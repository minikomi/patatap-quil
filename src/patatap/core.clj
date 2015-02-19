(ns patatap.core
  (:use [quil.core])
  (:require
    [quil.middleware :as m]
    ))

(def shape-count 2)

; //  easeInOutQuint: function (x, t, b, c, d) {
; //    if ((t/=d/2) < 1) return c/2*t*t*t*t*t + b;
; //    return c/2*((t-=2)*t*t*t*t + 2) + b;
; //  },

(defn in-out-quint [beginning change duration time-elapsed]
  (let [normalized-time (/ time-elapsed (/ duration 2))]
    (+ beginning
       (if (> 1 normalized-time)
         (* (/ change 2) (Math/pow normalized-time 4))
         (let [neg-norm-time (- normalized-time 2)]
           (* (/ change 2) (+ 2 (Math/pow neg-norm-time 5))))
         ))))

(defn easer-quint [beginning change duration]
  (partial in-out-quint beginning change duration))

;  if ((t/=d/2) < 1) return -c/2 * (Math.sqrt(1 - t*t) - 1) + b;
;                    return c/2 * (Math.sqrt(1 - (t-=2)*t) + 1) + b;

(defn in-out-circular [beginning change duration time-elapsed]
  
  (let [normalized-time (/ time-elapsed (/ duration 2))]
    (+ beginning
       (if (> 1 normalized-time)
         ; in
         (* (/ (- change) 2)
            (- (Math/sqrt (- 1 (* normalized-time normalized-time))) 1))
         ; out
         (let [neg-norm-time (- normalized-time 2)]
              (* (/ change 2)
                 (+ 1 (Math/sqrt (- 1 (* neg-norm-time neg-norm-time))))))))
    ))

(defn easer-circ [beginning change duration]
  (partial in-out-circular beginning change duration))

; millis for current miliseconds since sketch started.
; start time
; "distance" travelled 
; time taken to travel.

(defprotocol Shape 
  (draw [this] "draw this shape")
  (update [this] "update this shape")
  (alive [this] "should this shape remain?")
  )

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
                         )
         )))

(defn bump-up-shapes [current-shapes]
  (loop [cs current-shapes] 
    (if (<= shape-count (count cs)) cs
      (recur (conj cs (make-circle true) (make-circle false) )))))

(defn setup []
  {:current-shapes (bump-up-shapes [])})

(defn update-state [state]
  (update-in state [:current-shapes]
             #(->> %
                   (map update)
                   (filter alive)
                   (bump-up-shapes)
                   )))

(defn draw-state [state]
  (background 122 122 200)
  (blend-mode :)
  (doseq [s (:current-shapes state)] 
    (draw s)))

(defsketch patatap
  :title "patatap"
  :renderer :opengl
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
