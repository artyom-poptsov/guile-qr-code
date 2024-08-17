#!/usr/bin/env -S guile -L modules -e main -s
!#

;;; hello-world.scm -- Guile-QR-Code "hello world" program.

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

;; This program generates a QR Code with encoded "hello world" string and
;; saves the image to "hello-world.png" in the current directory.


;;; Code:

(use-modules (png)
             (qr-code))

(define (main args)
  "Entry point."
  (let* ((qr (qr-encode-text "hello world"))
         (img (qr-code->png-image qr)))
    (let ((port (open-output-file "hello-world.png")))
      (scm->png img port)
      (close port))))

;;; hello-world.scm ends here.
