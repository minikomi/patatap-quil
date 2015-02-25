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
                :bg-color (ui/->ToggleBox 90 20 "bg" false)
                }
   :show-ui true
   })


(defn handle-key [state event]
 (case (:key-code event)
  32 (update-in state [:show-ui] not)
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
                 [k (ui/handle-mousedown elem event)]))))

(defn handle-mouseup [state event]
  (assoc state :current-ui
         (into {}
               (for [[k elem] (:current-ui state)]
                 [k (ui/handle-mouseup elem event)]))))


(defn handle-mousemove [state event]
  (assoc state :current-ui
         (into {}
               (for [[k elem] (:current-ui state)]
                 [k (ui/handle-mousemove elem event)]))))

(defn update-state [{:keys [player beat] :as state}]

  (-> state
      (update-in [:current-shapes]
                 #(->> %
                       (map    s/update)
                       (filter s/alive)
                       (sort-by :z-index)
                       ))

      ))


(defn draw-state [state]
  (if (->> state :current-ui :bg-color :active)
    (fill 180)
    (fill 40 40 40 210))
  (rect 0 0 (width) (height))
  (blend-mode :blend)
  (doseq [s (:current-shapes state)] (s/draw s))
  (if (:show-ui state)
    (doseq [ui (vals (:current-ui state))] (ui/draw ui))))


(defn -main [& args]

  (sketch
  :title "patatap"
  :renderer :opengl
  :size :fullscreen
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :mouse-pressed  handle-mousedown
  :mouse-released  handle-mouseup
  :mouse-dragged  handle-mousemove
  :middleware [m/fun-mode]
  :features [:exit-on-close :present]
  ))
