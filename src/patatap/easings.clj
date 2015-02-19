(ns patatap.easings
  (:use [quil.core]))
 
; //  easeInOutQuint: function (x, t, b, c, d) {
; //    if ((t/=d/2) < 1) return c/2*t*t*t*t*t + b;
; //    return c/2*((t-=2)*t*t*t*t + 2) + b;
; //  },

(defn in-out-quint [beginning change duration time-elapsed]
  (if (<= duration time-elapsed) (+ beginning change)
    (let [normalized-time (/ time-elapsed (/ duration 2))]
    (+ beginning
       (if (> 1 normalized-time)
         (* (/ change 2) (Math/pow normalized-time 4))
         (let [neg-norm-time (- normalized-time 2)]
           (* (/ change 2) (+ 2 (Math/pow neg-norm-time 5))))
         )))))

(defn easer-quint [startval endval duration]
  (partial in-out-quint startval (- endval startval) duration))

;  if ((t/=d/2) < 1) return -c/2 * (Math.sqrt(1 - t*t) - 1) + b;
;                    return c/2 * (Math.sqrt(1 - (t-=2)*t) + 1) + b;

(defn in-out-circular [beginning change duration time-elapsed]
  
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

(defn easer-circ [startval endval duration]
  (partial in-out-circular startval (- startval endval) duration))


(defn delay-start [duration easer]
  (fn [t]
    (if (> duration t)
      (easer 0)
      (easer (- t duration))
      )))
