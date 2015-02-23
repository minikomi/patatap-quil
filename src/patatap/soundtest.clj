(ns patatap.soundtest
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [patatap.ui :as ui]
            )
  (:import (ddf.minim Minim)
           (ddf.minim.analysis FFT)
           ))

(defn setup []
  (let [m (Minim.)
        player (.loadFile m "/Users/adam/jddj3j.mp3")
        fft (FFT. (.bufferSize player) (.sampleRate player))]
    (.play player)
    {:player player :fft fft}
    ))

(defn draw-state [{:keys [player fft]}]
  (.forward fft (.mix player))
  (q/background 120 120 120 10)
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
        (q/rect (* v 10) (- 500 height) 10 height))
      )))

(q/defsketch yeah
  :title "player test"
  :size [512 500]
  :renderer :p3d
  :setup setup
  :on-close (fn [{:keys [player]}] (.pause player))
  :draw draw-state
  :middleware [m/fun-mode])
