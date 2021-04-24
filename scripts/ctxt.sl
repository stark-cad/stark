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