(ns patatap.soundtest2
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
        player (.loadFile m "resources/music/reflection.mp3")
        fft    (FFT. (.bufferSize player) (.sampleRate player))
        beat   (BeatDetect. (.bufferSize player) (.sampleRate player))
        ]
    (.play player)
    
    {
     :player player :fft fft :beat beat
     :kicksize 0
     :snaresize 0
     :hatsize 0
     }
    ))

(defn update [{:keys [fft hatsize snaresize kicksize beat player] :as state}]

  (.detectMode beat (. BeatDetect FREQ_ENERGY))
  (.setSensitivity beat 220)
  (.detect beat (.mix player))

  (assoc state 
         :kicksize
         (if (.isRange beat 0 6 4) (+ kicksize 30)
           (if (< 0 kicksize) 
             (* kicksize 0.8) 
             kicksize))
         :snaresize
         (if (.isRange beat 12 21 5)  
           (+ snaresize 30)
           (if (< 0 snaresize) 
             (* snaresize 0.8) 
             snaresize))
         :hatsize
         (if (.isRange beat 22 24 2) 
           (+ hatsize 30)
           (if (< 0 hatsize) 
             (* hatsize 0.8)
             hatsize))
         ))


(defn draw-state [{:keys [player fft kicksize snaresize hatsize]}]
  (.forward fft (.mix player))
  (q/background 120 120 120 10)
  (q/ellipse 100 250 kicksize kicksize)
  (q/ellipse 200 250 snaresize snaresize)
  (q/ellipse 300 250 hatsize hatsize)
  (let [bs (.bufferSize player)]
    (doseq [v (range 0 (dec bs))]
      (let [x1 (q/map-range v 0 bs 0 512)
            x2 (q/map-range (inc v) 0 bs 0 512)]
        (q/line x1 (+ 50 (* 50 (.. player left (get v)))) 
                x2 (+ 50 (* 50 (.. player left (get (inc v))))))
        (q/line x1 (+ 150 (* 50 (.. player right (get v)))) 
                x2 (+ 150 (* 50 (.. player right (get (inc v))))))))
    (doseq [v (range (.specSize fft))]
      (let [height (* (.getBand fft v) 6)] 
        (q/fill 255)
        (q/rect (* v 2) (- 500 height) 2 height))
      )))

(q/defsketch yeah
  :title "player test"
  :size [512 500]
  :renderer :p3d
  :setup setup
  :update update
  :on-close (fn [{:keys [player]}] (.pause player))
  :draw draw-state
  :middleware [m/fun-mode])
