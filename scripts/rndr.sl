; STARK, a system for computer augmented design.
; Copyright (C) 2021 Matthew Rothlisberger

; STARK is licensed under the terms of the GNU Affero General Public
; License. See the top level LICENSE file for the license text.

; Find full copyright information in the top level COPYRIGHT file.

; <>

; scripts/rndr.sl

; Main loop for the rendering thread of STARK's full graphical mode.

; <>

(def rndr (fn []

(def c-input ())
(def m-input ())

(def alive #T)

; currently tracking next line?
(def track #F)

(def line (arr-vec-make $f32 4 (as-f32 0.0)))

(def line-col (arr-vec-make $f32 3 (as-f32 0.0)))

(def get-q-next (fn [q] (def out ()) (while (eq out ()) (set out (qrx q))) out))

;(bg-col engine (as-f32 0.0) (as-f32 0.0) (as-f32 0.0))
(redraw engine)

(print "prepared for render loop")

(while alive
       ; TODO: avoid hardcoding this for lines here
       (if drawing (do (if track
                           (pop-line engine)
                           (set track #T))
                   (arr-vec-set line 0 (arr-vec-get point 0))
                   (arr-vec-set line 1 (arr-vec-get point 1))
                   (arr-vec-set line 2 (arr-vec-get cur-pos 0))
                   (arr-vec-set line 3 (arr-vec-get cur-pos 1))
                   (add-line engine line line-col))

                   (if track (do (pop-line engine) (set track #F)) ()))

       (set c-input (qrx cr-recv))
       (if (eq c-input :cx-dstr)
       (do
           (print "destroying render")
           (set alive #F))

       (if (eq c-input :cx-resz)
           (do (frame-size engine (arr-vec-get fr-dims 0) (arr-vec-get fr-dims 1)))

       (if (eq c-input :cx-rdrw)
           (do (redraw engine))

       ())))

       (set m-input (qrx mr-recv))
       (if (eq m-input :line)
           (do (if track (do (pop-line engine) (set track #F)) ())
               (arr-vec-set line 0 (get-q-next mr-recv))
               (arr-vec-set line 1 (get-q-next mr-recv))
               (arr-vec-set line 2 (get-q-next mr-recv))
               (arr-vec-set line 3 (get-q-next mr-recv))
               (add-line engine line line-col)
               (redraw engine))

       (if (eq m-input :line-col)
           (do (arr-vec-set line-col 0 (get-q-next mr-recv))
               (arr-vec-set line-col 1 (get-q-next mr-recv))
               (arr-vec-set line-col 2 (get-q-next mr-recv)))

       (if (eq m-input :back-col)
           (do (def r (get-q-next mr-recv))
               (def g (get-q-next mr-recv))
               (def b (get-q-next mr-recv))
               (bg-col engine r g b)
               (redraw engine))

       (if (eq m-input :clear)
           (do (clear engine)
               (redraw engine))

       (if (eq m-input :redraw)
           (do (redraw engine))

       ()))))))

(print "render end")

))
