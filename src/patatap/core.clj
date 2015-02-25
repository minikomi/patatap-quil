(ns patatap.core
  (:use [quil.core]
        )
  (:require
    [quil.middleware :as m]
    [patatap.shapes :as s]
    [patatap.ui :as ui]
    [clojure.java.io :as io]
    )

  (:import (ddf.minim Minim AudioListener)
           (ddf.minim.analysis FFT BeatDetect BeatListener)
           )
  (:gen-class :main true)
  )


(def file (atom "/usr/local/another.mp3"))

(defn setup []
  (smooth 8)
  (let [
        m      (Minim.)
        player (.loadFile m @file)
        fft    (FFT. (.bufferSize player) (.sampleRate player))
        beat   (BeatDetect. (.bufferSize player) (.sampleRate player))
        ]


   {:current-shapes []
    :current-ui {
                :music-play (ui/->ToggleBox 20 20 "music" false)

                :bg-color (ui/->ToggleBox 90 20 "bg" false)

                :delay (assoc (ui/->Slider 20 40 200 "delay" 10 500 240) :active false)

                :clay-min    (assoc (ui/->Slider 20 70 200 "clay-min"  0 26 0) :active false)
                :clay-max    (assoc (ui/->Slider 20 90 200 "clay-max"  0 26 5) :active false)
                :clay-thresh (assoc (ui/->Slider 20 110 200 "clay-thrs" 1 26 4) :active false)

                :prism-min    (assoc (ui/->Slider 20 140 200 "prism-min"  0 26 6) :active false)
                :prism-max    (assoc (ui/->Slider 20 160 200 "prism-max"  0 26 11) :active false)
                :prism-thresh (assoc (ui/->Slider 20 180 200 "prism-thrs" 1 26 4) :active false)

                :piston-min    (assoc (ui/->Slider 20 210 200 "piston-min"  0 26 12) :active false)
                :piston-max    (assoc (ui/->Slider 20 230 200 "piston-max"  0 26 16) :active false)
                :piston-thresh (assoc (ui/->Slider 20 250 200 "piston-thrs" 1 26 4) :active false)

                :confetti-min    (assoc (ui/->Slider 20 280 200 "confetti-min"  0 26 18) :active false)
                :confetti-max    (assoc (ui/->Slider 20 300 200 "confetti-max"  0 26 21) :active false)
                :confetti-thresh (assoc (ui/->Slider 20 320 200 "confetti-thrs" 1 26 3) :active false)

                }
    :player player :fft fft :beat beat
    :show-ui true
   }))


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

(defn update-state [{:keys [player beat] :as state}]
  ; use toggle to turn music on/off
  (if (:active (:music-play (:current-ui state)))
    (when (not (.isPlaying player))
      (.play player))
    (when (.isPlaying player)
      (doto player
        (.pause)
        (.rewind))))

  (.detect beat (.mix player))
  (.setSensitivity beat (->> state :current-ui :delay :current))

  (-> state
      (update-in [:current-shapes]
                 #(->> %
                       (map    s/update)
                       (filter s/alive)
                       (sort-by :z-index)
                       ))

      ((fn [s]
        (if (.isRange beat 
                      (->> state :current-ui :clay-min :current)  
                      (->> state :current-ui :clay-max :current)  
                      (->> state :current-ui :clay-thresh :current)  


                      )
          (update-in s [:current-shapes] 
                     conj 
                     (case (rand-int 20)
                       0 (s/make-wipe (< 0.5 (rand)))  
                       (s/make-clay 12)
                       ))
          s
          )))


      ((fn [s]
        (if (.isRange beat 
                      (->> state :current-ui :piston-min :current)  
                      (->> state :current-ui :piston-max :current)  
                      (->> state :current-ui :piston-thresh :current)  
                      )
          (update-in s [:current-shapes] 
                     conj 
                     (case (rand-int 4)
                       2 (s/make-piston 1 (< 0.5 (rand)))  
                       3 (s/make-piston 5 (< 0.5 (rand)))  
                       (s/make-piston 7 (< 0.5 (rand)))  
                       ))


          s
          )
        ))

      ((fn [s]
         (if (and (> 0.5 (rand))
                  (.isRange beat
                      (->> state :current-ui :prism-min :current)  
                      (->> state :current-ui :prism-max :current)  
                      (->> state :current-ui :prism-thresh :current)  
              ))

           (update-in s [:current-shapes] 
                      conj 
                      (case (rand-int 5)

                        0 (s/make-prism 3) 
                        1 (s/make-prism 4) 
                        2 (s/make-prism 6) 
                        (s/make-prism 5)


                        ))


           s
           )
        ))


      ((fn [s]
        (if (.isRange beat 
                      (->> state :current-ui :confetti-min :current)  
                      (->> state :current-ui :confetti-max :current)  
                      (->> state :current-ui :confetti-thresh :current)  
                      )
          (update-in s [:current-shapes] 
                     conj 
                     (case (rand-int 5)
                       (s/make-confetti 16)  


                       ))
          s
          )
        ))
      )
  )



(defn draw-state [state]
  (if (:active (:bg-color (:current-ui state))) 
    (fill 180)
    (fill 40 40 40 210))
  (rect 0 0 (width) (height))
  (blend-mode :blend)
  (doseq [s (:current-shapes state)] 
    (s/draw s))
  (if (:show-ui state)
    (doseq [ui (vals (:current-ui state))] 
      (ui/draw ui))))


(defn close-handler [{:keys [player]}]
  (.close player))

(defn -main [& args]
  (if (< 0 (count args)) (reset! file (first args)))

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
  :on-close   close-handler
  :features [:exit-on-close :present]
  ))
