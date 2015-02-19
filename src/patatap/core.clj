(ns patatap.core
  (:use [quil.core]
        [patatap.shapes]
        )
  (:require
    [quil.middleware :as m]))

(defn setup []
  {:current-shapes []})

(defn handle-key [state event]
 (case (:key-code event)
  65 (update-in state [:current-shapes] conj (make-wipe (< 0.5 (rand))))
  ;default - return unchanged state
  state
  ))

(defn update-state [state]
  (update-in state [:current-shapes]
             #(->> %
                   (map update)
                   (filter alive)
                   )))

(defn draw-state [state]
  (background 20)
  (blend-mode :blend)
  (doseq [s (:current-shapes state)] 
    (draw s)))

(defsketch patatap
  :title "patatap"
  :renderer :p2d
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :middleware [m/fun-mode])
