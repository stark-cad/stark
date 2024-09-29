; STARK, a system for computer augmented design.

; SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
; SPDX-License-Identifier: AGPL-3.0-only

; STARK is licensed under the terms of the GNU Affero General Public
; License version 3. See the top-level LICENSES directory for the
; license text.

; Find full copyright information in the top-level COPYRIGHT file.

; <>

; scripts/rndr.sl

; Main loop for the rendering thread of STARK's full graphical mode.

; <>

(def rndr (fn []

(def input ())

(def alive #T)

; currently tracking next line?
(def track #F)

(def line (arr-vec-make $f32 4 (as-f32 0.0)))

(def line-col (arr-vec-make $f32 3 (as-f32 0.0)))

(def get-q-next (fn [] (def out ()) (while (eq out ()) (set out (rest (qrx)))) out))

;(bg-col engine (as-f32 0.0) (as-f32 0.0) (as-f32 0.0))
(redraw engine)

(print "prepared for render loop")

(while alive
       ; TODO: avoid hardcoding this for lines here
       (if drawing (do (if track
                           (pop-line engine 0)
                           (set track #T))
                   (arr-vec-set line 0 (arr-vec-get point 0))
                   (arr-vec-set line 1 (arr-vec-get point 1))
                   (arr-vec-set line 2 (arr-vec-get cur-pos 0))
                   (arr-vec-set line 3 (arr-vec-get cur-pos 1))
                   (add-line engine 0 line line-col))

                   (if track (do (pop-line engine 0) (set track #F)) ()))

       (set input (rest (qrx)))
       (if (eq input :line-add)
           (do (if track (do (pop-line engine 0) (set track #F)) ())
               (arr-vec-set line 0 (get-q-next))
               (arr-vec-set line 1 (get-q-next))
               (arr-vec-set line 2 (get-q-next))
               (arr-vec-set line 3 (get-q-next))
               (add-line engine 0 line line-col)
               (redraw engine))

       (if (eq input :line-pop)
           (do (pop-line engine 0) (redraw engine))

       (if (eq input :line-col)
           (do (arr-vec-set line-col 0 (get-q-next))
               (arr-vec-set line-col 1 (get-q-next))
               (arr-vec-set line-col 2 (get-q-next)))

       (if (eq input :back-col)
           (do (def r (get-q-next))
               (def g (get-q-next))
               (def b (get-q-next))
               (bg-col engine r g b)
               (redraw engine))

       (if (eq input :clear)
           (do (clear engine)
               (redraw engine))

       (if (eq input :redraw)
           (do (redraw engine))

       (if (eq input :cx-dstr)
       (do
           (print "destroying render")
           (set alive #F))

       (if (eq input :cx-resz)
           (do (frame-size engine (arr-vec-get fr-dims 0) (arr-vec-get fr-dims 1)))

       (if (eq input :cx-rdrw)
           (do (redraw engine))

       (if (eq input :cx-crmv)
           (do (if track (redraw engine) ()))

       ()))))))))))
       )

(print "render end")

))
