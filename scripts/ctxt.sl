; STARK, a system for computer augmented design.
; Copyright (C) 2021 Matthew Rothlisberger

; STARK is licensed under the terms of the GNU Affero General Public
; License. See the top level LICENSE file for the license text.

; Find full copyright information in the top level COPYRIGHT file.

; <>

; scripts/ctxt.sl

; Important functions for the context thread of STARK's full graphical
; mode.

; <>

(do

(def ctx-dst (fn []

(print "context destroy")
(qtx cm-send :destroy)
(qtx cr-send :destroy)

))


(def ctx-rsz (fn [w h]

(print "context resize")
(qtx cr-send :resize)
(qtx cr-send w)
(qtx cr-send h)

))


(def ctx-clk (fn [x y]

(print "context click")
(qtx cm-send :point)
(qtx cm-send x)
(qtx cm-send y)

))

)