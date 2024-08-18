#!/usr/bin/env -S guile -L modules -e main -s
!#

;;; ascii-art.scm -- Print a QR code to console.

;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This program generates a QR Code with encoded the given string and
;; prints it to the stdout as an ASCII art image.
;;
;; Usage:
;;   ./ascii-art.scm <some-text> <size>
;;
;; Example:
;;   ./ascii-art.scm "hello world" 64


;;; Code:

(use-modules (png)
             (qr-code))

(define (main args)
  "Entry point."
  (when (< (length args) 3)
    (error "Text data and QR code size must be provided." args))
  (let* ((data           (cadr args))
         (size           (string->number (caddr args)))
         (qr-code        (qr-encode-text data))
         (qr-code-string (qr-code->string qr-code #:size size)))
    (display qr-code-string)
    (newline)))

;;; hello-world.scm ends here.
