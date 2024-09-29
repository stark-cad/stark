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

(def get-q-next (fn [] (def out ()) (while (eq out ()) (set out (rest (qrx)))) out))

(def cur-pos-set (fn [x y] (cursor-pos frame (arr-vec-get fr-dims 0)
                                             (arr-vec-get fr-dims 1)
                                             x y)))

(def cur-pos-mod (fn [op x y] (cur-pos-set (op (arr-vec-get cur-pos 0) x)
                                           (op (arr-vec-get cur-pos 1) y))))

(def line-col-set (fn [r g b] (qtx rdr-tgt :line-col)
                              (qtx rdr-tgt (as-f32 r))
                              (qtx rdr-tgt (as-f32 g))
                              (qtx rdr-tgt (as-f32 b))))

(def back-col-set (fn [r g b] (qtx rdr-tgt :back-col)
                              (qtx rdr-tgt (as-f32 r))
                              (qtx rdr-tgt (as-f32 g))
                              (qtx rdr-tgt (as-f32 b))))

(def lines-clear (fn [] (qtx rdr-tgt :clear)))

(def line-f32 (fn [x1 y1 x2 y2]
                      (qtx rdr-tgt :line-add)
                      (qtx rdr-tgt x1) (qtx rdr-tgt y1)
                      (qtx rdr-tgt x2) (qtx rdr-tgt y2)))

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
       (set input (rest (qrx)))

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
           (do (print (eval (parse (rest input)))))

       (if (eq input :cx-kb-u)
           (do (cur-pos-mod - (as-f32 0.0) step)
               (qtx rdr-tgt :redraw))

       (if (eq input :cx-kb-d)
           (do (cur-pos-mod + (as-f32 0.0) step)
               (qtx rdr-tgt :redraw))

       (if (eq input :cx-kb-f)
           (do (cur-pos-mod + step (as-f32 0.0))
               (qtx rdr-tgt :redraw))

       (if (eq input :cx-kb-b)
           (do (cur-pos-mod - step (as-f32 0.0))
               (qtx rdr-tgt :redraw))

       (if (eq input :cx-kb-l)
           (do (set step (* step (as-f32 2.0))))

       (if (eq input :cx-kb-s)
           (do (set step (/ step (as-f32 2.0))))

       ; TODO: move track out of rndr
       (if (eq input :cx-kb-e)
           (do (set drawing #F)
               (qtx rdr-tgt :redraw)
               (qtx rdr-tgt :redraw))

       (if (eq input :cx-kb-k)
           (do (qtx rdr-tgt :line-pop))

       (if (eq input :cx-kb-m)
           (do (if (eq draw-fn line-f32)
                   (set draw-fn rect-f32)
                   (set draw-fn line-f32)))

       ())))))))))))))

(print "main end")

))
