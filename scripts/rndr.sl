(def rndr (fn []

(def c-input ())
(def m-input ())

(def alive #T)

(def get-q-next (fn [q] (def out ()) (while (eq out ()) (set out (qrx q))) out))

(print "prepared for render loop")

(while alive
       (set c-input (qrx cr-recv))
       (if (eq c-input :destroy)
       (do
           (print "destroying render")
           (set alive #F))
       (if (eq c-input :resize)
           (do (frame-size engine (get-q-next cr-recv) (get-q-next cr-recv))
               (print "frame sized")
               (redraw engine)) ()))

       (set m-input (qrx mr-recv))
       (if (eq m-input :line)
           (do (def line (vec-f32-make))
               (vec-f32-push line (get-q-next mr-recv))
               (vec-f32-push line (get-q-next mr-recv))
               (vec-f32-push line (get-q-next mr-recv))
               (vec-f32-push line (get-q-next mr-recv))
               (new-line engine line)
               (redraw engine)) ()))

(print "render end")

))