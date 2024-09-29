; STARK, a system for computer augmented design.

; SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
; SPDX-License-Identifier: AGPL-3.0-only

; STARK is licensed under the terms of the GNU Affero General Public
; License version 3. See the top-level LICENSES directory for the
; license text.

; Find full copyright information in the top-level COPYRIGHT file.

; <>

; scripts/main.sl

; Main loop for the manager thread of STARK's full graphical mode.

; <>

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

(def cur-pos-mod (fn [op x y] (cur-pos-set (op (arr-vec-get cur-pos 0) x)
                                           (op (arr-vec-get cur-pos 1) y))))

(def line-col-set (fn [r g b] (qtx mr-send :line-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def back-col-set (fn [r g b] (qtx mr-send :back-col)
                              (qtx mr-send (as-f32 r))
                              (qtx mr-send (as-f32 g))
                              (qtx mr-send (as-f32 b))))

(def lines-clear (fn [] (qtx mr-send :clear)))

(def line-f32 (fn [x1 y1 x2 y2]
                      (qtx mr-send :line-add)
                      (qtx mr-send x1) (qtx mr-send y1)
                      (qtx mr-send x2) (qtx mr-send y2)))

(def rect-f32 (fn [x1 y1 x2 y2]
                  (line-f32 x1 y1 x2 y1)
                  (line-f32 x1 y1 x1 y2)
                  (line-f32 x2 y2 x1 y2)
                  (line-f32 x2 y2 x2 y1)))

(def draw-fn line-f32)

; Draw function for REPL use
(def draw (fn [x1 y1 x2 y2]
              (draw-fn (as-f32 x1) (as-f32 y1)
                       (as-f32 x2) (as-f32 y2))))

(def step (as-f32 0.0625))

(print "prepared for main loop")

(while alive
       (set input (qrx cm-recv))

       (if (eq input :cx-dstr) (do
           (print "destroying main")
           (set alive #F))

       (if (eq input :cx-rcrd) (do
           (if drawing (do
               (set drawing #F)
               (draw-fn (arr-vec-get point 0) (arr-vec-get point 1)
                        (arr-vec-get cur-pos 0) (arr-vec-get cur-pos 1)))
           (do (set drawing #T)
               (arr-vec-set point 0 (arr-vec-get cur-pos 0))
               (arr-vec-set point 1 (arr-vec-get cur-pos 1)))))

       (if (eq input :cx-shel)
           (do (print (eval (parse (get-q-next cm-recv)))))

       (if (eq input :cx-kb-u)
           (do (cur-pos-mod - (as-f32 0.0) step)
               (qtx mr-send :redraw))

       (if (eq input :cx-kb-d)
           (do (cur-pos-mod + (as-f32 0.0) step)
               (qtx mr-send :redraw))

       (if (eq input :cx-kb-f)
           (do (cur-pos-mod + step (as-f32 0.0))
               (qtx mr-send :redraw))

       (if (eq input :cx-kb-b)
           (do (cur-pos-mod - step (as-f32 0.0))
               (qtx mr-send :redraw))

       (if (eq input :cx-kb-l)
           (do (set step (* step (as-f32 2.0))))

       (if (eq input :cx-kb-s)
           (do (set step (/ step (as-f32 2.0))))

       ; TODO: move track out of rndr
       (if (eq input :cx-kb-e)
           (do (set drawing #F)
               (qtx mr-send :redraw)
               (qtx mr-send :redraw))

       (if (eq input :cx-kb-k)
           (do (qtx mr-send :line-pop))

       (if (eq input :cx-kb-m)
           (do (if (eq draw-fn line-f32)
                   (set draw-fn rect-f32)
                   (set draw-fn line-f32)))

       ())))))))))))))

(print "main end")

))
