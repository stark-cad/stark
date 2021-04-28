; STARK, a system for computer augmented design.
; Copyright (C) 2021 Matthew Rothlisberger

; STARK is licensed under the terms of the GNU Affero General Public
; License. See the top level LICENSE file for the license text.

; Find full copyright information in the top level COPYRIGHT file.

; <>

; scripts/main.sl

; Main loop for the manager thread of STARK's full graphical mode.

; <>

(def main (fn []

(def input ())
(def drawing #F)
(def alive #T)

(def point (vec-f32-make))
(vec-f32-push point (as-f32 0.0))
(vec-f32-push point (as-f32 0.0))

(def get-q-next (fn [q] (def out ()) (while (eq out ()) (set out (qrx q))) out))

(def line-col-set (fn [r g b] (qtx mr-send :line-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def back-col-set (fn [r g b] (qtx mr-send :back-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def clear-lines (fn [] (qtx mr-send :clear)))

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