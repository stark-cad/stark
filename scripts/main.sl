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

(def prot-rec-proc (eval (parse (temp-read-file "scripts/prot_line_rec.sl"))))

(def child-records [])
(def cur-child ())

(def inst-window (fn []
     (def new-thr (th-spawn prot-rec-proc))
     (vec-push child-records new-thr)
     (qtx rdr-tgt (link :get-win (link (th-id new-thr) (rest new-thr))))
     (qtx (rest new-thr) (link :ini-rdt rdr-tgt))
     (qtx (rest new-thr) (link :ini-cxt (own-tx-hdl)))
))

(def vec-find (fn [target pred]
     (def idx 0)
     (def found #F)
     (def entry ())
     (while (and (not found) (not (= idx (vec-len target))))
            (set entry (vec-get target idx))
            (if (pred entry) (set found #T) ())
            (set idx (+ 1 idx)))
     (if found entry ())))

(inst-window)

(print "prepared for main loop")

(def cur-win ())
(def last-ht-pt ())

(def cm-sig :cx-crmv)

(while alive
       ; TODO: blocking queue receive which parks the thread / strand
       (set input (qrx))

       (cond
         (not input) ()

           (= input rdr-id) (do
              (set input (rest input))
              (cond (eq input :win-ret) (do
                 (def wh (rest input))
                 (def id (rest wh))
                 (def slot (vec-find child-records (fn [e] (= id (th-id e)))))
                 (if (not (eq slot ())) (do
                     (link (rest slot) wh)
                     (qtx (rest slot) (link :ini-whd wh))) ()))

           ; TODO: in most cases, first use of def in a loop shadows all to follow!!
                 (eq input :win-hit) (do
                     (set cur-win (rest input))
                     (set last-ht-pt (rest cur-win))
                     (set cur-child (vec-find child-records (fn [e] (= cur-win (rest (rest e)))))))
           ))

         (= input 1) (do
           (set input (rest input))
           (cond
                (eq input :cx-crmv) (do
                    (qtx rdr-tgt (link :hit-win cur-pos)))

                (eq input :cx-dstr) (do
                    (print "destroying main")
                    (set alive #F))

                (eq input :cx-shel) (do
                    (print (eval (parse (rest input)))))

                (eq input :cx-rcrd) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-u) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-d) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-f) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-b) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-l) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-s) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-e) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-k) (do
                    (if cur-child (qtx (rest cur-child) input) ()))

                (eq input :cx-kb-m) (do
                    (if cur-child (qtx (rest cur-child) input) ()))
           ))

         (= input (th-id cur-child)) (do
             (set input (rest input))
             (if (eq input :cmv-req) (do
                 (print "cursor move requested, lol"))
          ()))
))

(print "main end")

))
