;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2024 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/prot_line_rec.sl

;; Controlling logic for a live instance of the prototype line record.

;; <>

;; TODO: load whole file with inherent separation from loading context
(fn []
    ;; input comes from common manager thread (initialization, human interaction)

    ;; at least for now, EVERYTHING comes over the queue. no shared state!

    ;; output as dispatches to common render thread (draw commands)

(def line-f32 (fn [wh x1 y1 x2 y2]
                  (def out (arr-vec-make $f32 4 (as-f32 0.0)))
                  (def sigil :line-add)
                  (arr-vec-set out 0 x1)
                  (arr-vec-set out 1 y1)
                  (arr-vec-set out 2 x2)
                  (arr-vec-set out 3 y2)
                  (qtx rdr-vsl (link sigil (link wh out)))))

(def rect-f32 (fn [wh x1 y1 x2 y2]
                  (line-f32 wh x1 y1 x2 y1)
                  (line-f32 wh x1 y1 x1 y2)
                  (line-f32 wh x1 y2 x2 y2)
                  (line-f32 wh x2 y1 x2 y2)))

(def draw-fn line-f32)

(def cur-pos-set (fn [x y] (def newpos (arr-vec-make $f32 2 0.0))
                           (arr-vec-set newpos 0 x)
                           (arr-vec-set newpos 1 y)
                           (qtx cxi-hdl (link :cmv-req newpos))))

(def cur-pos-mod (fn [op x y] (cur-pos-set (op (arr-vec-get curp 0) x)
                                           (op (arr-vec-get curp 1) y))))

(def input ())

(def fresh #T)
(def alive #F)

(def drawing #F)

(def curp (arr-vec-make $f32 2 (as-f32 0.0)))
(def point (arr-vec-make $f32 2 (as-f32 0.0)))

(def step (as-f32 0.0625))

(def rdr-vsl ())
(def cxi-hdl ())
(def wnd-id ())

; get initialization data
(while fresh
       (set input (rest (qrx)))

       (cond
        (eq input :ini-whd) (do (print "got handle!") (set wnd-id (rest input)))
        (eq input :ini-rdt) (do (print "got rdr-tgt!") (set rdr-vsl (rest input)))
        (eq input :ini-cxt) (do (print "got cxi-tgt!") (set cxi-hdl (rest input))))

       ; TODO: we can see that the language needs some work...
       (if (and (not (eq wnd-id ())) (and (not (eq rdr-vsl ())) (not (eq cxi-hdl ()))))
           (do (set fresh #F)
               (set alive #T)
               (print "initialized!")) ())
)

; main loop
(while alive
       ;; TODO: Sail equivalent to match
       (set input (rest (qb)))

       (cond
         (eq input :cx-crmv) (do
           (set curp (rest input))
;           (tmp-coord-log curp)
           (if drawing (do
               (qtx rdr-vsl (link :line-pop wnd-id))
               (line-f32 wnd-id
                         (arr-vec-get point 0) (arr-vec-get point 1)
                         (arr-vec-get curp 0) (arr-vec-get curp 1))) ()))

       ; TODO: drawing very slow, reduce computation / communication per op
         (eq input :cx-rcrd) (do
           (print wnd-id)
           (print "click received")
           (if drawing
             (do
               (set drawing #F)
               (qtx rdr-vsl (link :line-pop wnd-id))
               (draw-fn wnd-id (dbg (arr-vec-get point 0)) (dbg (arr-vec-get point 1))
                                (dbg (arr-vec-get curp 0)) (dbg (arr-vec-get curp 1))))
             (do
               (set drawing #T)
               (arr-vec-set point 0 (arr-vec-get curp 0))
               (arr-vec-set point 1 (arr-vec-get curp 1))
               (line-f32 wnd-id (arr-vec-get curp 0) (arr-vec-get curp 1)
                                 (arr-vec-get curp 0) (arr-vec-get curp 1)))))

         (eq input :cx-kb-u)
           (do (cur-pos-mod - (as-f32 0.0) step)
               (qtx rdr-vsl :redraw))

         (eq input :cx-kb-d)
           (do (cur-pos-mod + (as-f32 0.0) step)
               (qtx rdr-vsl :redraw))

         (eq input :cx-kb-f)
           (do (cur-pos-mod + step (as-f32 0.0))
               (qtx rdr-vsl :redraw))

         (eq input :cx-kb-b)
           (do (cur-pos-mod - step (as-f32 0.0))
               (qtx rdr-vsl :redraw))

         (eq input :cx-kb-l)
           (do (set step (* step (as-f32 2.0))))

         (eq input :cx-kb-s)
           (do (set step (/ step (as-f32 2.0))))

         (eq input :cx-kb-e)
           (do (set drawing #F)
               (qtx rdr-vsl (link :line-pop wnd-id))
               (qtx rdr-vsl :redraw))

         (eq input :cx-kb-k)
         (do (qtx rdr-vsl (link :line-pop wnd-id)))

         (eq input :cx-kb-m)
           (do (if (eq draw-fn line-f32)
                   (set draw-fn rect-f32)
                   (set draw-fn line-f32)))
      )
))
