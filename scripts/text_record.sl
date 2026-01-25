;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2025 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/text_record.sl

;; Sail module which provides a CRDT for a string of text. The
;; underlying data and operations for a text record.

;; <>

;; (mod @text-rec)
;; (deftype $record-head)
;; prototype element: [ins-stamp ?del-stamp character]

;; TODO: alt syntax for indexing? vec[idx] seems unambiguous
;; TODO: copy entire structure when invoking a function?

;; NOTE: initializer, two operations, one query

(def init (fn []
              '()))

;; TODO: tail call optimization (track identifier for proc?)
(def insert (fn [self stamp tgt char]
                (def elt (vec-new))
                (vec-push elt stamp)
                (vec-push elt ())
                (vec-push elt char)

                (def ins-inner
                     (fn [sum cur]
                         (if (vec-get cur 1) (ins-inner sum (rest cur))
                           (if (>= sum tgt)
                               (do
                                (if (rest cur) (link elt (rest cur)) ())
                                (link cur elt))
                             (ins-inner (+ 1 sum) (rest cur))))))

                (if (or (= tgt 0) (elp self)) (push self elt)
                  (ins-inner 1 (fst self)))))

(def delete (fn [self stamp tgt]
                (def del-inner
                     (fn [sum cur]
                         (if (vec-get cur 1) (del-inner sum (rest cur))
                           (if (>= sum tgt)
                               (vec-set cur 1 stamp)
                             (del-inner (+ 1 sum) (rest cur))))))

                (del-inner 0 (fst self))))

;; (set (vec-get cur 1) stamp)))

;; TODO: set resolves first argument to a pointer to a SlHndl slot,
;; allowing for general use to set symbol values or modify collections

(def as-str (fn [self]
                (def cur (if (elp self) self (fst self)))
                (def acc (str-new))
                (while cur
                  (if (not (vec-get cur 1))
                      (vec-push acc (vec-get cur 2))
                    ())
                  (set cur (rest cur)))
                acc))

;; NOTE: this logic all belongs in the record task

(def next-stamp (fn [self])
     ;; TODO: prototype: add 1 to current last stamp
     )

(def ins-local (fn [self tgt char]
                 (insert self (next-stamp self) tgt char)))

(def del-local (fn [self tgt]
                 (delete self (next-stamp self) tgt)))

(def interface [[:op 1 apply-op] [:ins 2 ins-local] [:del 1 del-local] [:xstr 0 as-str]])

(def apply-op (fn [self op]))

;; [:ins <stamp> <tgt> <char>]

;; (record.insert 157 'a')
;; (.insert record 157 #'a)

;; (.push vec 'a')
