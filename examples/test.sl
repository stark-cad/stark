; Project Euler Problem 1 Solution
; run time, mid March 2021: >70 seconds
; run time, April 13 2021: ~4 seconds

(do
(def mult (fn [max acc cur] (if (= cur max)
                      acc
                      (do (if (= (mod cur 3) 0)
                              (def acc (+ acc cur))
                              (if (= (mod cur 5) 0) (def acc (+ acc cur)) ()))
                      ;(print cur)
                      (mult max acc (+ cur 1))))))
(mult 1000 0 1)
)