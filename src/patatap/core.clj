(ns patatap.core
  (:use [quil.core]
        )
  (:require
    [quil.middleware :as m]
    [patatap.shapes :as s]
    [patatap.ui :as ui]
    )
  (:gen-class :main true)
  )


(defn setup []
  (smooth 8)
  {:current-shapes []
   :current-ui {
                :toggle (ui/->ToggleBox 20 20 "test" false)
                :slider (assoc (ui/->Slider 20 40 200 "test" 0 5 0) :active false)

                }
   })


(defn handle-key [state event]
 (case (:key-code event)
  65 (update-in state [:current-shapes] conj (s/make-wipe (< 0.5 (rand))))
  83 (update-in state [:current-shapes] conj (s/make-veil (< 0.5 (rand))))
  68 (update-in state [:current-shapes] conj (s/make-prism 3))
  70 (update-in state [:current-shapes] conj (s/make-prism 4))
  71 (update-in state [:current-shapes] conj (s/make-prism 5))
  72 (update-in state [:current-shapes] conj (s/make-clay 12))
  74 (update-in state [:current-shapes] conj (s/make-piston 1 (< 0.5 (rand))))
  75 (update-in state [:current-shapes] conj (s/make-piston 4 (< 0.5 (rand))))
  76 (update-in state [:current-shapes] conj (s/make-piston 7 (< 0.5 (rand))))
  77 (update-in state [:current-shapes] conj (s/make-confetti 16))

  ;default - return unchanged state
  state
  ))

(defn handle-mousedown [state event]
  ; check each ui to see if down was within, mark it as active
  (assoc state :current-ui
         (into {}
               (for [[k elem] (:current-ui state)]
                 [k (ui/handle-mousedown elem event)])))
  
  )

(defn handle-mouseup [state event]
  (assoc state :current-ui
         (into {}
               (for [[k elem] (:current-ui state)]
                 [k (ui/handle-mouseup elem event)])))

  )


(defn handle-mousemove [state event]
  (assoc state :current-ui
         (into {}
               (for [[k elem] (:current-ui state)]
                 [k (ui/handle-mousemove elem event)])))

  )

(defn update-state [state]
  (update-in state [:current-shapes]
             #(->> %
                   (map    s/update)
                   (filter s/alive)
                   (sort-by :z-index)
                   )))

(defn draw-state [state]
  (background 180)
  (blend-mode :blend)
  (doseq [s (:current-shapes state)] 
    (s/draw s))
  (doseq [ui (vals (:current-ui state))] 
    (ui/draw ui)))

(defn -main []
  (defsketch patatap
  :title "patatap"
  :renderer :opengl
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :mouse-pressed  handle-mousedown
  :mouse-released  handle-mouseup
  :mouse-dragged  handle-mousemove
  :middleware [m/fun-mode]))
