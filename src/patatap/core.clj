(ns patatap.core
  (:use [quil.core]
        [patatap.shapes]
        )
  (:require
    [quil.middleware :as m]))

(defn setup []
  (smooth 8)
  {:current-shapes []})

(defn handle-key [state event]
 (case (:key-code event)
  65 (update-in state [:current-shapes] conj (make-wipe (< 0.5 (rand))))
  83 (update-in state [:current-shapes] conj (make-veil (< 0.5 (rand))))
  68 (update-in state [:current-shapes] conj (make-prism 3))
  70 (update-in state [:current-shapes] conj (make-prism 4))
  71 (update-in state [:current-shapes] conj (make-prism 5))
  72 (update-in state [:current-shapes] conj (make-clay 12))
  74 (update-in state [:current-shapes] conj (make-piston 1 (< 0.5 (rand))   ))
  75 (update-in state [:current-shapes] conj (make-piston 4 (< 0.5 (rand))   ))
  76 (update-in state [:current-shapes] conj (make-piston 7 (< 0.5 (rand))   ))
  77 (update-in state [:current-shapes] conj (make-confetti 16))

  ;default - return unchanged state
  state
  ))

(defn update-state [state]
  (update-in state [:current-shapes]
             #(->> %
                   (map update)
                   (filter alive)
                   (sort-by :z-index)
                   )))

(defn draw-state [state]
  (background 180)
  (blend-mode :blend)
  (doseq [s (:current-shapes state)] 
    (draw s)))

(defsketch patatap
  :title "patatap"
  :renderer :opengl
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :middleware [m/fun-mode])
