(ns patatap.soundtest3
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [patatap.ui :as ui]
            )
  (:import (ddf.minim Minim AudioListener)
           (ddf.minim.analysis FFT BeatDetect BeatListener)
           )
  
  )

(defn setup []
  (let [
        m      (Minim.)
        player (.loadFile m "resources/music/another.mp3")
        fft    (FFT. (.bufferSize player) (.sampleRate player))
        beat   (BeatDetect. (.bufferSize player) (.sampleRate player))
        ]
    (.play player)

    
    {
     :events []
     :beat beat
     :player player
     }
    ))

(defn update [{:keys [events beat player] :as state}]

  (.detectMode beat (. BeatDetect FREQ_ENERGY))
  (.setSensitivity beat 220)
  (.detect beat (.mix player))
  
  (-> state

      (update-in [:events]
                 conj
                 (reduce 
                   (fn [o n]
                     (if (.isRange beat (* n 3) (+ (* n 3) 3) 3)
                       (conj o (* 30 n))
                       o
                       )) []
                   (range 6)
                   ))
      ((fn [s] (if (< 200 (count (:events s)))
                 (update-in s [:events] subvec 1)
                 s
                 ))))


  )


(defn draw-state [{:keys [events]}]
  (q/background 10)
  (doseq [evframe (map (fn [evf n] {:frame evf :n n}) events (range))
          ev (:frame evframe)]


      (q/ellipse (+ 10 (* 10 (:n evframe))) (+ 20 ev) 10 30)
    
    )



  )

(q/defsketch yeah
  :title "player test"
  :size [2000 500]
  :renderer :p3d
  :setup setup
  :update update
  :on-close (fn [{:keys [player]}] (.pause player))
  :draw draw-state
  :middleware [m/fun-mode])
