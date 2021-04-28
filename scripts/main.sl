(def main (fn []

(def input ())
(def drawing #F)
(def alive #T)

(def point (vec-f32-make))
(vec-f32-push point (as-f32 0.0))
(vec-f32-push point (as-f32 0.0))

(def get-q-next (fn [q] (def out ()) (while (eq out ()) (set out (qrx q))) out))

(print "prepared for main loop")

(while alive
       (set input (qrx cm-recv))
       (if (eq input :destroy) (do
           (print "destroying main")
           (set alive #F))

       (if (eq input :point) (do
           (print "got point in")
           (if drawing (do
               (qtx mr-send :line)
               (qtx mr-send (vec-f32-get point 0))
               (qtx mr-send (vec-f32-get point 1))
               (qtx mr-send (get-q-next cm-recv))
               (qtx mr-send (get-q-next cm-recv))
               (print "points sent")
               (set drawing #F))

           (do (set drawing #T)
               (print "loading point")
               (vec-f32-set point 0 (get-q-next cm-recv))
               (vec-f32-set point 1 (get-q-next cm-recv)))))
       ())))

(print "main end")

))