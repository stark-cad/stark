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

(def source ())
(def input ())

(def alive #T)

(def line (arr-vec-make $f32 4 (as-f32 0.0)))

(def line-col (arr-vec-make $f32 3 (as-f32 0.0)))

(def get-q-next (fn [] (def out ()) (while (eq out ()) (set out (rest (qrx)))) out))

; TODO: iterators? transducers?

(def vec-find (fn [target pred]
     (def idx 0)
     (def found #F)
     (def entry ())
     (while (and (not found) (not (= idx (vec-len target))))
            (set entry (vec-get target idx))
            (if (pred entry) (set found #T) ())
            (set idx (+ 1 idx)))
     (if found entry ())))

(def windows [])

;(bg-col engine (as-f32 0.0) (as-f32 0.0) (as-f32 0.0))
(redraw engine)

(print mgr-id)
(print "prepared for render loop")

(def reported-pt ())
(def ht-result ())
(def ht-cur ())

(def ht-last 0) ; temp

(def cm-sig :cx-crmv)
(def ht-sig :win-hit)

; TODO: fix insane variable shadowing
(while alive
       (set source (qrx))
       (set input (rest source))

       (cond
         (not source) ()

         (and (= source mgr-id) (eq input :get-win)) (do
           (def for-id (rest input))
           (def new-win (create-window engine))
           ; (modify-window engine new-win (as-f32 -1.0) (as-f32 -1.0) (as-f32 0.0) (as-f32 1.0))
           (vec-push windows (link new-win for-id))
           (qtx mgr-tgt (link :win-ret new-win)))

         (and (= source mgr-id) (eq input :hit-win)) (do
           (set reported-pt (rest input))
           (set ht-result (hit-test engine (arr-vec-get reported-pt 0) (arr-vec-get reported-pt 1)))
           (qtx mgr-tgt (link ht-sig ht-result)))

           ;TODO: linking :win-hit to ht-result ONLY works the first time (???)
           ;TODO: linking through a symbol works fine (???)

         (eq input :line-add) (do
           (def atgt (rest input))
;           (if (= source (rest (vec-find windows (fn [e] (= e atgt))))) )
           (if (= source (tmp-vec-match windows atgt))
               (add-line engine atgt (rest atgt) line-col) ())
           (redraw engine))

         (eq input :line-pop) (do
           (def ptgt (rest input))
;           (if (= source (rest (vec-find windows (fn [e] (= e ptgt))))) )
           (if (= source (tmp-vec-match windows ptgt))
               (pop-line engine ptgt) ())
           (redraw engine))

         (eq input :line-col)
           (do (arr-vec-set line-col 0 (get-q-next))
               (arr-vec-set line-col 1 (get-q-next))
               (arr-vec-set line-col 2 (get-q-next)))

         (eq input :back-col)
           (do (def r (get-q-next))
               (def g (get-q-next))
               (def b (get-q-next))
               (bg-col engine r g b)
               (redraw engine))

         (eq input :clear)
           (do (clear engine)
               (redraw engine))

         (eq input :redraw)
           (do (redraw engine))

         (eq input :cx-dstr) (do
           (print "destroying render")
           (set alive #F))

         (eq input :cx-resz)
           (do (frame-size engine (arr-vec-get fr-dims 0) (arr-vec-get fr-dims 1)))

         (eq input :cx-rdrw)
           (do (redraw engine))

       ; TODO: modify shared state indicating the current window and cursor position?
         (eq input :cx-crmv) (do
           (set ht-result (hit-test engine (tmp-coord-log (rest input))))
           (if (not (= ht-last ht-result)) (do
               (qtx mgr-tgt (link ht-sig ht-result))
               (set ht-cur (vec-find windows (fn [e] (= e ht-result))))) ())
           (set ht-last ht-result)
           (if ht-cur (qtx (rest (rest ht-cur)) (link cm-sig (rest ht-result))) ()))
       ))

(print "render end")

))
