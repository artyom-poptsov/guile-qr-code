;;; qr.scm -- QR code generator.

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

;; Guile-QR-Code provides facilities to generate QR codes. [1]
;;
;; [1] https://en.wikipedia.org/wiki/QR_code


;;; Code:

(define-module (qr-code)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (png)
  #:use-module (png image)
  #:use-module (png graphics)
  #:use-module (qr-code encoder)
  #:export (qr-encode-text
            qr-encode-binary
            qr-code->png-image))



(define-with-docs %default-qr-code-size
  "Default size of a QR-code."
  1024)



(define* (qr-encode-text data #:key (ecl ECC-LOW))
  "Generate a QR code based on a text @var{data}.  Return a QR Code."
  (encode-text data ecl))

(define* (qr-encode-binary data #:key (ecl ECC-LOW))
  "Generate a QR code based on a binary @var{data}.  Return a QR Code."
  (encode-binary data ecl))

(define* (qr-code->png-image qr-code
                             #:key
                             (size %default-qr-code-size)
                             (foreground-color #vu8(0 0 0))
                             (background-color #vu8(255 255 255)))
  "Convert a @var{qr-code} to a PNG image.  This procedure requires Guile-PNG."
  (let* ((modules (QR-code-modules qr-code))
         (image (make <png-image>
                  #:color-type 2
                  #:bit-depth  8
                  #:width      size
                  #:height     size))
         (first-row (vector-ref modules 0))
         (rows-count (vector-length modules))
         (module-size (inexact->exact
                       (floor/ size
                               (vector-length first-row)))))
    (draw! image
           (make <filled-rectangle>
             #:color    background-color
             #:position (make <point> #:x 0 #:y 0)
             #:height   size
             #:width    size))
    (format (current-error-port) "module size: ~a~%"
            module-size)

    (let row-loop ((row-index 0))
      (unless (= row-index rows-count)
        (let* ((row (vector-ref modules row-index))
               (row-length (vector-length row)))
          (let column-loop ((column-index 0))
            (unless (= column-index row-length)
              (let ((module (vector-ref row column-index)))
                (when module
                  (draw! image
                         (make <filled-rectangle>
                           #:color foreground-color
                           #:position (make <point>
                                        #:x (* column-index module-size)
                                        #:y (* row-index module-size))
                           #:width module-size
                           #:height module-size)))
                (column-loop (+ column-index 1)))))
          (row-loop (+ row-index 1)))))
    image))

;;; qr.scm ends here.
