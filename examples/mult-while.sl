; SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
; SPDX-License-Identifier: AGPL-3.0-only

; Project Euler Problem 1 Solution
; using Sail's while form

; run time, Apr 21 2021: ~0.07 s
; run time, Oct 10 2022: negligible
; run time, Apr 23 2023: ~0.01 s
; run time, Jul 26 2023: ~0.05 s
; run time, Sep 27 2024: ~0.01 s

(def max 1000)
(def acc 0)
(def cur 1)
(while (not (= cur max))
       (if (= (mod cur 3) 0)
           (set acc (+ acc cur))
           (if (= (mod cur 5) 0)
               (set acc (+ acc cur))
               ()))
       (set cur (+ cur 1)))
;(print acc)
acc
