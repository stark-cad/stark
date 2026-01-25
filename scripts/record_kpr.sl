;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2025 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/record_kpr.sl

;; Process which maintains the ground truth for a single record, an
;; unit of design data, and serves query responses about it

;; <>

(def keeper []
     ;; instance has full record information (metadata and design
     ;; data); also has full record type information (query
     ;; procedures, operation procedures, interface documentation)

     ;; can first receive information relevant to record type
     ;; (procedures, supported operation and query keywords, mapping
     ;; between them)

     (def apply (fn [p al] (eval (push al p))))

     ;; TODO: extremely basic default / universal record procedures

     ;; these report the entire available interface for this specific
     ;; record task
     (def report-queries (fn [rec]))
     (def report-ops (fn [rec]))

     ;; these report on static details of the record data
     (def report-cdx-id (fn [rec]))
     (def report-replica-id (fn [rec]))
     (def report-type (fn [rec]))

     ;; table which associates operation / query names with
     ;; procedures; <keyword param-count procedure>

     ;; (def interface (@text-rec/interface))
     ;; (def interface (vec-new))

     (eval (parse-prog (temp-read-file "scripts/text_record.sl")))

     ;; (def record-data (@text-rec/init))
     (def record-data (init))

     ;; TODO: resolve the `set` and `def` mess
     ;; NOTE: 'this symbol has already been defined'?

     (while #T
       (set input (qrx))

       (def source (fst qrx))
       (def request (rest qrx))

       (def match (vec-find interface (fn [e] (= (vec-get e 1) request))))

       ;; TODO: (assert (= (.len (rest qrx)) (vec-get match 1)))

       (def result (apply (vec-get match 2) (rest request)))

       (qtx (task-target-request source) result)

       ;; dispatch message: it is either a query or an operation

       ;; default (universal to records) queries handled here from metadata

       ;; type-specific queries checked against record-type's
       ;; supported query list; appropriate query procedure invoked on
       ;; record data with provided arguments

       ;; operations checked against record-type's supported operation
       ;; list; appropriate operation procedure invoked on record data
       ;; with provided arguments

       ;; message sent to requesting vessel with query results or
       ;; operation receipt

       ;; messages sent to all subscribed vessels (with operation
       ;; shorthand?) on every record data change
       )
)
