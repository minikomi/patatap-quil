(ns patatap.core
  (:use [quil.core]
        )
  (:require
    [quil.middleware :as m]
    [patatap.shapes :as s]
    [patatap.ui :as ui]
    [overtone.osc :as osc]
    [clojure.core.async :as async :refer [<! put! chan go]]
    )

  (:gen-class :main true)
  )

(def shapes (atom []))

(defn setup []
  (smooth 8)
  (frame-rate 30)
  (let [server (osc/osc-server 1337)
        osc-chan (chan)
        ]

    (osc/osc-handle server "/kick"  (fn [_] (put! osc-chan {:msg :kick})))
    (osc/osc-handle server "/snare" (fn [_] (put! osc-chan {:msg :snare})))
    (osc/osc-handle server "/hat"   (fn [_] (put! osc-chan {:msg :hat})))

    (go 
      (loop [msg (:msg (<! osc-chan))]
        (case msg
          :kick  (if (< 0.5 (rand))
                   (swap! shapes conj (case (rand-int 10)
                                      1 (s/make-wipe (< 0.5 (rand)))
                                      2 (s/make-veil (< 0.5 (rand)))
                                      (s/make-clay 12))))
          :snare (if (> 0.5 (rand)) (swap! shapes conj (case (rand-int 7)
                                    0 (s/make-prism 3)  
                                    1 (s/make-prism 4)  
                                    2 (s/make-prism 5)  
                                    3 (s/make-prism 6)  
                                    4 (s/make-piston 1 (< 0.5 (rand)))   
                                    5 (s/make-piston 4 (< 0.5 (rand)))   
                                    6 (s/make-piston 7 (< 0.5 (rand)))   
                                     (s/make-piston 4 (< 0.5 (rand)))))) 
          :hat   (if (< 0.3 (rand)) (swap! shapes conj (s/make-confetti 12))) 
        )
        (recur (:msg (<! osc-chan)))))

    {:current-ui {
                  :bg-color (ui/->ToggleBox 90 20 "bg" false)
                  }
     :show-ui true
     }
  ))


(defn handle-key [state event]
 (case (:key-code event)
  32 (update-in state [:show-ui] not)
  65 (do (swap! shapes  conj (s/make-wipe (< 0.5 (rand)))) state)
  83 (do (swap! shapes  conj (s/make-veil (< 0.5 (rand)))) state)
  68 (do (swap! shapes  conj (s/make-prism 3)) state)
  70 (do (swap! shapes  conj (s/make-prism 4)) state)
  71 (do (swap! shapes  conj (s/make-prism 5)) state)
  72 (do (swap! shapes  conj (s/make-clay 12)) state)
  74 (do (swap! shapes  conj (s/make-piston 1 (< 0.5 (rand)))) state)
  75 (do (swap! shapes  conj (s/make-piston 4 (< 0.5 (rand)))) state)
  76 (do (swap! shapes  conj (s/make-piston 7 (< 0.5 (rand)))) state)
  77 (do (swap! shapes  conj (s/make-confetti 16)) state)

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

(defn update-state [state]
  (swap! shapes
         #(->> %
               (map    s/update)
               (filter s/alive)
               (sort-by :z-index)
               ))
  state
  )


(defn draw-state [state]
  (if (->> state :current-ui :bg-color :active)
    (background 180)
    (background 20))
  (doseq [s @shapes] (s/draw s))
  ; (save-frame "######.tga")
  )


(defn -main [& args]

  (sketch
  :title "patatap"
  :renderer :opengl
  :size [1920 1080]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :mouse-pressed  handle-mousedown
  :mouse-released  handle-mouseup
  :mouse-dragged  handle-mousemove
  :middleware [m/fun-mode]
  :features [:exit-on-close]
  ))
