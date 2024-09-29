; SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
; SPDX-License-Identifier: AGPL-3.0-only

; Project Euler Problem 1 Solution
; using tail recursion

; run time, late Mar 2021: >70 s
; run time, Apr 21 2021: ~3.3 s
; run time, Oct 10 2022: ~0.02 s
; run time, Apr 23 2023: ~0.09 s
; run time, Jul 26 2023: ~1.7 s
; run time, Sep 27 2024: ~0.23 s

(def mult (fn [max acc cur]
              (if (= cur max)
                  acc
                  (do (if (= (mod cur 3) 0)
                          (set acc (+ acc cur))
                          (if (= (mod cur 5) 0)
                              (set acc (+ acc cur))
                              ()))
                      (mult max acc (+ cur 1))))))
(mult 1000 0 1)
