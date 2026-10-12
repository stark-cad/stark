;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2025 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/codex_mgr.sl

;; Process representing an entire codex, a collection of records which
;; may directly reference one another.

;; <>

(def manager
     (fn [codex]
         (def input ())
         ;; knows the full list (index) of records, which ones are live,
         ;; the keeper vessels thereof, and their queue inlet handles
         ;; (message targets)

         (print "codex manager spawned with:")
         (print codex)

         ;; start by spawning keeper vessel for default record, whose
         ;; data will be passed as an argument

         ;; also spawn a buffer vessel linked to that record, and seek
         ;; a window for it to render into

         (def default-record (alist-get (fst codex) (rest codex)))

         (eval (parse-file "scripts/record_kpr.sl"))

         (def drec (vsl-spawn keeper (list default-record)))

         ;; responsible for instantiating record keepers
         (while #T
           (set input (qb))

           )
         ))
