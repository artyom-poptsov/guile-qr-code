;;; reed-solomon.scm -- Reed-Solomon error correction

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

(define-module (qr-code core reed-solomon)
  #:use-module (rnrs bytevectors)
  #:use-module (qr-code core bits)
  #:export (reed-solomon-multiply
            reed-solomon-compute-divisor
            reed-solomon-compute-remainder))

(define (reed-solomon-multiply x y)
	; Returns the product of the two given field elements modulo GF(2^8/0x11D). The arguments and result
	; are unsigned 8-bit integers. This could be implemented as a lookup table of 256*256 entries of uint8.

	(unless (and (<= 0 x 255) (<= 0 y 255))
		(error "Byte out of range"))

	; Russian peasant multiplication
	(do ((i 7 (- i 1))
	     (z 0 (let ((nz (bit-xor (* z 2) (* (quotient z 128) #x11D))))
	                 (if (bit-set? i y) (bit-xor nz x) nz))))
		((negative? i) z)))

(define (reed-solomon-compute-divisor degree)
	; Returns a Reed-Solomon ECC generator polynomial for the given degree. This could be
	; implemented as a lookup table over all possible parameter values, instead of as an algorithm.

	(unless (<= 1 degree 255)
		(error "Degree out of range"))

	; Polynomial coefficients are stored from highest to lowest power, excluding the leading term which is always 1.
	; For example the polynomial x^3 + 255x^2 + 8x + 93 is stored as the uint8 array [255, 8, 93].
  	; Start off with the monomial x^0
	(define result (make-bytevector degree 0))
	(bytevector-u8-set! result (- degree 1) 1)

	; Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
	; and drop the highest monomial term which is always 1x^degree.
	; Note that r = 0x02, which is a generator element of this field GF(2^8/0x11D).
	(do ((root 1 (reed-solomon-multiply root 2))
             (i    0 (+ i 1)))
		((= i degree))
		(do ((j 0 (+ j 1)))
			((= j degree))
			(let* ((rj0 (bytevector-u8-ref result j))
			       (rj1 (reed-solomon-multiply rj0 root))
			       (nxj (+ j 1))
			       (rj2 (if (>= nxj degree)
					rj1
					(bit-xor rj1 (bytevector-u8-ref result nxj)))))
				(bytevector-u8-set! result j rj2))))
	result)

(define  (reed-solomon-compute-remainder data divisor)
	; Returns the Reed-Solomon error correction codeword for the given data and divisor polynomials.

	(define result (make-bytevector (bytevector-length divisor) 0))
	(do ((i 0 (+ i 1)))
		((>= i (bytevector-length data)))
		(let ((factor (bit-xor (bytevector-u8-ref data i) (bytevector-u8-ref result 0))))
			(do ((u 0 v)(v 1 (+ v 1)))
				((>= v (bytevector-length result)) (bytevector-u8-set! result u 0))
				(bytevector-u8-set! result u (bytevector-u8-ref result v)))
			(do ((u 0 (+ u 1)))
				((>= u (bytevector-length divisor)))
				(bytevector-u8-set! result u
					(bit-xor
						(bytevector-u8-ref result u)
						(reed-solomon-multiply factor (bytevector-u8-ref divisor u)))))))
	result)

;;; reed-solomon.scm ends here.
