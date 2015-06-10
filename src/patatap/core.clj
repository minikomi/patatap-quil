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
     (osc/osc-handle server "/snare"   ( fn [_] ( put! osc-chan  {:msg 1})))
     (osc/osc-handle server "/hat"     ( fn [_] ( put! osc-chan {:msg 2})))
     (osc/osc-handle server "/kick"    ( fn [_] ( put! osc-chan  {:msg 3})))
     (osc/osc-handle server "/piano"   ( fn [_] ( put! osc-chan {:msg 4})))
     (osc/osc-handle server "/accent1" ( fn [_] ( put! osc-chan  {:msg 5})))
     (osc/osc-handle server "/accent2" ( fn [_] ( put! osc-chan  {:msg 6})))
     (osc/osc-handle server "/accent3" ( fn [_] ( put! osc-chan {:msg 7})))

     (go 
       (loop [msg (:msg (<! osc-chan))]
         (case msg
           1 (swap! shapes 
                    (fn [shps]
                      (conj
                        (filterv #(not (= (:type %) :piston)) shps)
                        (case (rand-int 5)
                          0 (s/make-piston 1( < 0.5 ( rand)))
                          1 (s/make-piston 7( < 0.5 ( rand)))
                          (s/make-frisbee)))))
           2 (swap! shapes 
                    (fn [shps] 
                      (conj
                        (filterv #(not (= (:type %) :confetti)) shps)    
                        (s/make-confetti 10))))
           3 (swap! shapes 
                    (fn [shps] 
                      (conj
                        (filterv #(not (= (:type %) :clay)) shps)   
                        (s/make-clay 12))))
           4 (swap! shapes
                    (fn [shps] 
                      (conj
                        (filterv #(not (= (:type %) :prism)) shps)   
                        (s/make-prism (+ 3 (* 2 (rand-int 3))) )
                        )))
           5 (swap! shapes conj (s/make-glimmer 18))
           6 (swap! shapes conj (s/make-wipe ( < 0.5 ( rand))))
           7 (swap! shapes conj (s/make-donut))
           )
        (recur (:msg (<! osc-chan)))))

    {:current-ui {
                  :bg-color (ui/->ToggleBox 90 20 "bg" false)
                  }
     :show-ui true
     :render false
     :server server
     }
  ))


(defn handle-key [state event]
  (case (:raw-key event)
  \u (update-in state [:show-ui] not)
  \r (update-in state [:render] not)
  \a (do (swap! shapes  conj (s/make-wipe (< 0.5 (rand)))) state)
  \s (do (swap! shapes  conj (s/make-veil (< 0.5 (rand)))) state)
  \d (do (swap! shapes  conj (s/make-prism 3)) state)
  \f (do (swap! shapes  conj (s/make-prism 4)) state)
  \g (do (swap! shapes  conj (s/make-prism 5)) state)
  \h (do (swap! shapes  conj (s/make-clay 12)) state)
  \j (do (swap! shapes  conj (s/make-piston 1 (< 0.5 (rand)))) state)
  \k (do (swap! shapes  conj (s/make-piston 4 (< 0.5 (rand)))) state)
  \l (do (swap! shapes  conj (s/make-piston 7 (< 0.5 (rand)))) state)
  \z (do (swap! shapes  conj (s/make-confetti 16)) state)
  \x (do (swap! shapes  conj (s/make-glimmer 18)) state)
  \c (do (swap! shapes  conj (s/make-donut)) state)
  \v (do (swap! shapes  conj (s/make-frisbee)) state)

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
  (background (if (-> state :current-ui :bg-color :active) 60 181))
  (doseq [s @shapes] (s/draw s))
  (when (:show-ui state) 
    (doseq [u (-> state :current-ui vals)] (ui/draw u))
    (when (:render state)
      (fill 255 0 0)
      (rect 0 0 20 20)
      ))
  (when (:render state)
    (save-frame "frames/frame-######.tga"))
  )

(defn on-close [state]
  (osc/osc-close (:server state))
  (System/exit 0) 
  )

(defn -main [& args]
  (sketch
  :title "patatap"
  :renderer :opengl
  :size [1920 1080]
  :setup setup
  :update update-state
  :draw draw-state
  :on-close on-close
  :key-pressed handle-key
  :mouse-pressed  handle-mousedown
  :mouse-released  handle-mouseup
  :mouse-dragged  handle-mousemove
  :middleware [m/fun-mode]
  ))
