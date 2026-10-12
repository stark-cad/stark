;; STARK, a system for computer augmented design.

;; SPDX-FileCopyrightText: © 2026 Matthew Rothlisberger
;; SPDX-License-Identifier: AGPL-3.0-only

;; STARK is licensed under the terms of the GNU Affero General Public
;; License version 3. See the top-level LICENSES directory for the
;; license text.

;; Find full copyright information in the top-level COPYRIGHT file.

;; <>

;; scripts/prot_text_buf.sl

;; Basic input and output modes for interacting with a text record.

;; <>

;; the buffer is expected to be assigned a window for output, and to
;; directly receive input events which may come while its window is
;; focused

;; turn key presses into record operations

;; display the current text in the window
