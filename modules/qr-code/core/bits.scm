;;; bits.scm -- Bits manipulation procedures.

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
;;
;; This file incorporates work of José Bollo titled "QR Code generator library
;; (Scheme R7RS)" (see <https://gitlab.com/jobol/qrcode-gen>) covered by the
;; following copyright and permission notice:
;;
;; QR Code generator library (Scheme R7RS)
;;
;; SPDX-License-Identifier: MIT
;; Author: jobol
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; - The above copyright notice and this permission notice shall be included in
;;   all copies or substantial portions of the Software.
;; - The Software is provided "as is", without warranty of any kind, express or
;;   implied, including but not limited to the warranties of merchantability,
;;   fitness for a particular purpose and noninfringement. In no event shall the
;;   authors or copyright holders be liable for any claim, damages or other
;;   liability, whether in an action of contract, tort or otherwise, arising from,
;;   out of or in connection with the Software or the use or other dealings in the
;;   Software.
;;


;;; Commentary:

;; TODO:


;;; Code:

(define-module (qr-code core bits)
  #:use-module (srfi srfi-11)
  #:export (bit-set?
            bit-xor
            bit-or))

(define (bit-set? idx n)
  (define (cuth mask a)
    (if (> mask a)
        a
        (let ((na (cuth (+ mask mask) a)))
          (if (> mask na) na (- na mask)))))
  (define (bset idx mask)
    (if (positive? idx)
        (bset (- idx 1) (+ mask mask))
        (>= (cuth (+ mask mask) n) mask)))
  (bset idx 1))

(define (bit-xor a b)
  (define (bxor mask a b)
    (if (or (and (< a mask) (< b mask)) (zero? mask))
        (values 0 a b)
        (let-values (((r na nb) (bxor (+ mask mask) a b)))
          (if (< na mask)
              (if (< nb mask)
                  (values r na nb)
                  (values (+ r mask) na (- nb mask)))
              (if (< nb mask)
                  (values (+ r mask) (- na mask) nb)
                  (values r (- na mask) (- nb mask)))))))
  (let-values (((r x y) (bxor 1 a b))) r))

(define (bit-or a b)
  (define (bor mask a b)
    (if (or (and (< a mask) (< b mask)) (zero? mask))
        (values 0 a b)
        (let-values (((r na nb) (bor (+ mask mask) a b)))
          (if (< na mask)
              (if (< nb mask)
                  (values r na nb)
                  (values (+ r mask) na (- nb mask)))
              (values (+ r mask) (- na mask) (if (< nb mask) nb (- nb mask)))))))
  (let-values (((r x y) (bor 1 a b))) r))

;;; bits.scm ends here.
