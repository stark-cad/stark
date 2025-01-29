; STARK, a system for computer augmented design.

; SPDX-FileCopyrightText: © 2024 Matthew Rothlisberger
; SPDX-License-Identifier: AGPL-3.0-only

; STARK is licensed under the terms of the GNU Affero General Public
; License version 3. See the top-level LICENSES directory for the
; license text.

; Find full copyright information in the top-level COPYRIGHT file.

; <>

; scripts/prot_line_rec.sl

; Controlling logic for a live instance of the prototype line record.

; <>

;; input comes from common manager thread (initialization, human interaction)

;; at least for now, EVERYTHING comes over the queue. no shared state!

;; output as dispatches to common render thread (draw commands)

(fn []

(def line-f32 (fn [wh x1 y1 x2 y2]
                  (def out (arr-vec-make $f32 4 (as-f32 0.0)))
                  (def sigil :line-add)
                  (arr-vec-set out 0 x1)
                  (arr-vec-set out 1 y1)
                  (arr-vec-set out 2 x2)
                  (arr-vec-set out 3 y2)
                  (qtx rdr-tgt (link sigil (link wh out)))))

(def rect-f32 (fn [wh x1 y1 x2 y2]
                  (line-f32 wh x1 y1 x2 y1)
                  (line-f32 wh x1 y1 x1 y2)
                  (line-f32 wh x2 y2 x1 y2)
                  (line-f32 wh x2 y2 x2 y1)))

(def draw-fn line-f32)

(def cur-pos-set (fn [x y] (def newpos (arr-vec-make $f32 2 0.0))
                           (arr-vec-set newpos 0 x)
                           (arr-vec-set newpos 1 y)
                           (qtx cxi-tgt (link :cmv-req newpos))))

(def cur-pos-mod (fn [op x y] (cur-pos-set (op (arr-vec-get curp 0) x)
                                           (op (arr-vec-get curp 1) y))))

(def input ())

(def fresh #T)
(def alive #F)

(def drawing #F)

(def curp (arr-vec-make $f32 2 (as-f32 0.0)))
(def point (arr-vec-make $f32 2 (as-f32 0.0)))

(def step (as-f32 0.0625))

(def rdr-tgt ())
(def cxi-tgt ())
(def wnd-hdl ())

; get initialization data
(while fresh
       (set input (rest (qrx)))

       (cond
        (eq input :ini-whd) (do (print "got handle!") (set wnd-hdl (rest input)))
        (eq input :ini-rdt) (do (print "got rdr-tgt!") (set rdr-tgt (rest input)))
        (eq input :ini-cxt) (do (print "got cxi-tgt!") (set cxi-tgt (rest input))))

       ; TODO: we can see that the language needs some work...
       (if (and (not (eq wnd-hdl ())) (and (not (eq rdr-tgt ())) (not (eq cxi-tgt ()))))
           (do (set fresh #F)
               (set alive #T)
               (print "initialized!")) ())
)

; main loop
(while alive
       ;; TODO: Sail equivalents to cond and match
       (set input (rest (qrx)))

       (cond
         (eq input :cx-crmv) (do
           (set curp (rest input))
           (tmp-coord-log curp)
           (if drawing (do
               (qtx rdr-tgt (link :line-pop wnd-hdl))
               (line-f32 wnd-hdl
                         (arr-vec-get point 0) (arr-vec-get point 1)
                         (arr-vec-get curp 0) (arr-vec-get curp 1))) ()))

       ; TODO: drawing very slow, reduce computation / communication per op
         (eq input :cx-rcrd) (do
           (print "click received")
           (if drawing
             (do
               (set drawing #F)
               (qtx rdr-tgt (link :line-pop wnd-hdl))
               (draw-fn wnd-hdl (dbg (arr-vec-get point 0)) (dbg (arr-vec-get point 1))
                                (dbg (arr-vec-get curp 0)) (dbg (arr-vec-get curp 1))))
             (do
               (set drawing #T)
               (arr-vec-set point 0 (arr-vec-get curp 0))
               (arr-vec-set point 1 (arr-vec-get curp 1))
               (line-f32 wnd-hdl (arr-vec-get curp 0) (arr-vec-get curp 1)
                                 (arr-vec-get curp 0) (arr-vec-get curp 1)))))

         (eq input :cx-kb-u)
           (do (cur-pos-mod - (as-f32 0.0) step)
               (qtx rdr-tgt :redraw))

         (eq input :cx-kb-d)
           (do (cur-pos-mod + (as-f32 0.0) step)
               (qtx rdr-tgt :redraw))

         (eq input :cx-kb-f)
           (do (cur-pos-mod + step (as-f32 0.0))
               (qtx rdr-tgt :redraw))

         (eq input :cx-kb-b)
           (do (cur-pos-mod - step (as-f32 0.0))
               (qtx rdr-tgt :redraw))

         (eq input :cx-kb-l)
           (do (set step (* step (as-f32 2.0))))

         (eq input :cx-kb-s)
           (do (set step (/ step (as-f32 2.0))))

         (eq input :cx-kb-e)
           (do (set drawing #F)
               (qtx rdr-tgt (link :line-pop wnd-hdl))
               (qtx rdr-tgt :redraw))

         (eq input :cx-kb-k)
           (do (qtx rdr-tgt (link :line-pop wnd-hdl)))

         (eq input :cx-kb-m)
           (do (if (eq draw-fn line-f32)
                   (set draw-fn rect-f32)
                   (set draw-fn line-f32)))
      )
))
