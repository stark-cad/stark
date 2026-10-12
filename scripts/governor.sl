;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/governor.sl

;; Main loop for the governor vessel of STARK's full graphical mode.

;; <>

(def drawing #F)
(def point (arr-vec-make $f32 2 (as-f32 0.0)))

(def gov (fn []

(def input ())
(def alive #T)

;; alist associating record ID to record data, followed by default record ID
(def default-codex '(((1 ([0 () #'t] [1 () #'e] [2 () #'s] [3 () #'t]))) 1))

(eval (parse-file "scripts/codex_mgr.sl"))

(def prot-rec-proc (eval (parse-file "scripts/prot_line_rec.sl")))

(def child-records [])
(def cur-child ())

(def gw-sig :get-win)

(def inst-window (fn []
     (def new-vsl (vsl-spawn prot-rec-proc '()))
     (vec-push child-records new-vsl)
     (qtx rdr-vsl (link gw-sig new-vsl))
     (qtx new-vsl (link :ini-rdt rdr-vsl))
     (qtx new-vsl (link :ini-cxt (own-hdl)))
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

(def cdx (vsl-spawn manager (list default-codex)))

(inst-window)

(print "prepared for governor loop")

(def cur-win ())
(def last-ht-pt ())

(def cm-sig :cx-crmv)
(def iw-sig :ini-whd)

(def wh 0)
(def id 0)
(def slot ())

(while alive
  (set input (qb))

  (cond
   (not input) ()

   (= input (vsl-id rdr-vsl))
   (do
    (set input (rest input))
    (cond (eq input :win-ret)
          (do
           (set wh (rest input))
           (print wh)
           (set id (rest wh))
           (set slot (vec-find child-records (fn [e] (= id (vsl-id e)))))
           (if (not (eq slot ()))
               (do
                (link slot wh)
                (qtx slot (link iw-sig wh))) ()))

          ;; TODO: fix first use of def in a loop shadowing all to follow
          (eq input :win-hit)
          (do
           (set cur-win (rest input))
           (print cur-win)
           (set last-ht-pt (rest cur-win))
           (set cur-child (vec-find child-records (fn [e] (= cur-win (rest e))))))
          ))

   (= input 1)
   (do
    (set input (rest input))
    (cond
     (eq input :cx-crmv) (do
                          (qtx rdr-vsl (link :hit-win cur-pos)))

                (eq input :cx-dstr) (do
                    (print "destroying governor")
                    (set alive #F))

                (eq input :cx-shel) (do
                    (print (eval (parse (rest input)))))

                (eq input :cx-rcrd) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-u) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-d) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-f) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-b) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-l) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-s) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-e) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-k) (do
                    (if cur-child (qtx cur-child input) ()))

                (eq input :cx-kb-m) (do
                    (if cur-child (qtx cur-child input) ()))
           ))

   (= input (vsl-id cur-child))
   (do
    (set input (rest input))
    (if (eq input :cmv-req)
        (do
         (print "cursor move requested, lol"))
      ()))
))

(print "governor end")

))
