; STARK, a system for computer augmented design.
; Copyright (C) 2021 Matthew Rothlisberger

; STARK is licensed under the terms of the GNU Affero General Public
; License. See the top level LICENSE file for the license text.

; Find full copyright information in the top level COPYRIGHT file.

; <>

; scripts/main.sl

; Main loop for the manager thread of STARK's full graphical mode.

; <>

(do

(def drawing #F)
(def point (arr-vec-make $f32 2 (as-f32 0.0)))

(def main (fn []

(def input ())
(def alive #T)

; (def drawing #F)
; (def point (arr-vec-make $f32 2 (as-f32 0.0)))

(def get-q-next (fn [q] (def out ()) (while (eq out ()) (set out (qrx q))) out))

(def cur-pos-set (fn [x y] (cursor-pos frame (arr-vec-get fr-dims 0)
                                             (arr-vec-get fr-dims 1)
                                             x y)))

(def line-col-set (fn [r g b] (qtx mr-send :line-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def back-col-set (fn [r g b] (qtx mr-send :back-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def lines-clear (fn [] (qtx mr-send :clear)))

(def line-add-f32 (fn [x1 y1 x2 y2]
                      (qtx mr-send :line)
                      (qtx mr-send x1) (qtx mr-send y1)
                      (qtx mr-send x2) (qtx mr-send y2)))

(def line-add (fn [x1 y1 x2 y2]
                  (line-add-f32 (as-f32 x1) (as-f32 y1)
                                (as-f32 x2) (as-f32 y2))))

(print "prepared for main loop")

(while alive
       (set input (qrx cm-recv))

       (if (eq input :cx-dstr) (do
           (print "destroying main")
           (set alive #F))

       (if (eq input :cx-rcrd) (do
           (if drawing (do
               (set drawing #F)
               (line-add-f32 (arr-vec-get point 0) (arr-vec-get point 1)
                             (arr-vec-get cur-pos 0) (arr-vec-get cur-pos 1)))
           (do (set drawing #T)
               (arr-vec-set point 0 (arr-vec-get cur-pos 0))
               (arr-vec-set point 1 (arr-vec-get cur-pos 1)))))

       (if (eq input :cx-shel)
           (do (print (eval (parse (get-q-next cm-recv)))))

       ()))))

(print "main end")

))

)
