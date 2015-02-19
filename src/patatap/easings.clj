(ns patatap.easings
  (:use [quil.core]))
 

(defn delay-start [duration easer]
  (fn [t]
    (if (> duration t)
      (easer 0)
      (easer (- t duration))
      )))

(defn in-out-quint [beginning change duration time-elapsed]
  ; if ((t/=d/2) < 1) return c/2*t*t*t*t*t + b;
  ;                   return c/2*((t-=2)*t*t*t*t + 2) + b;
  (if (<= duration time-elapsed) (+ beginning change)
    (let [normalized-time (/ time-elapsed (/ duration 2))]
    (+ beginning
       (if (> 1 normalized-time)
         (* (/ change 2) (Math/pow normalized-time 4))
         (let [neg-norm-time (- normalized-time 2)]
           (* (/ change 2) (+ 2 (Math/pow neg-norm-time 5))))
         )))))

(defn easer-in-out-quint [startval endval duration]
  (partial in-out-quint startval (- endval startval) duration))


(defn in-out-circular [beginning change duration time-elapsed]
  ;  if ((t/=d/2) < 1) return -c/2 * (Math.sqrt(1 - t*t) - 1) + b;
  ;                    return c/2 * (Math.sqrt(1 - (t-=2)*t) + 1) + b;
  (if (<= duration time-elapsed) (+ beginning change)
    (let [normalized-time (/ time-elapsed (/ duration 2))]
      (+ beginning
         (if (> 1 normalized-time)
           ; in
           (* (/ (- change) 2)
              (- (Math/sqrt (- 1 (* normalized-time normalized-time))) 1))
           ; out
           (let [neg-norm-time (- normalized-time 2)]
             (* (/ change 2)
                (+ 1 (Math/sqrt (- 1 (* neg-norm-time neg-norm-time))))))))
      )))

(defn easer-in-out-circ [startval endval duration]
  (partial in-out-circular startval (- endval startval) duration))


(defn in-circular [beginning change duration time-elapsed]
  ; return -c * (Math.sqrt(1 - (t/=d)*t) - 1) + b;
  (if (<= duration time-elapsed) (+ beginning change) 
   (let [normalized-time (/ time-elapsed duration)]
    (+ beginning
       (* (- change)
          (- (Math/sqrt (- 1 (* normalized-time normalized-time))) 1)
          )))))

(defn easer-in-circ [startval endval duration]
  (partial in-circular startval (- endval startval) duration))
