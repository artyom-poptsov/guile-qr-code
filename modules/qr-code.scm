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
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (qr-code encoder)
  #:re-export (ECC-LOW
               ECC-MEDIUM
               ECC-QUARTILE
               ECC-HIGH)
  #:export (qr-encode-text
            qr-encode-binary
            qr-code->png-image
            qr-code->string))



(define-with-docs %default-qr-code-size
  "Default size of a QR-code."
  1024)

(define-with-docs %default-qr-code-ascii-size
  "Default size of a QR-code."
  64)

(define-with-docs %no-color
  "Reset terminal colors."
  (string-append (string #\esc) "[0m"))



(define* (qr-encode-text data #:key (ecl ECC-LOW))
  "Generate a QR code based on a text @var{data}.  Return a QR Code."
  (encode-text data ecl))

(define* (qr-encode-binary data #:key (ecl ECC-LOW))
  "Generate a QR code based on a binary @var{data}.  Return a QR Code."
  (encode-binary data ecl))

(define* (qr-code->string qr-code
                          #:key
                          (size %default-qr-code-ascii-size)
                          (foreground-char #\█)
                          (background-char #\space)
                          (width-scale-factor 2)
                          (height-scale-factor 1)
                          (foreground-color #f)
                          (background-color #f)
                          (margin 2))
  "Convert a @var{qr-code} to an ASCII art image."
  (let* ((modules (QR-code-modules qr-code))
         (first-row (vector-ref modules 0))
         (row-length (vector-length first-row))
         (rows-count (vector-length modules))
         (module-size (inexact->exact
                       (floor/ size
                               (vector-length first-row)))))

    (with-output-to-string
      (lambda ()
        (let ((width (- (* size width-scale-factor)
                        (* margin 2))))
          (for-each (lambda _
                      (write-line (string-append
                                   background-color
                                   (make-string width background-char)
                                   %no-color)))
                    (iota (* margin height-scale-factor))))
        (let row-loop ((row-index 0)
                       (result    '()))
          (if (= row-index rows-count)
              (for-each (lambda (text-row)
                          (for-each (lambda (k)
                                      (let ((text-row (string-append
                                                       background-color
                                                       (make-string (* margin width-scale-factor)
                                                                    background-char)
                                                       %no-color
                                                       text-row
                                                       background-color
                                                       (make-string (* margin width-scale-factor)
                                                                    background-char)
                                                       %no-color)))
                                        (write-line text-row)))
                                    (* (iota module-size)
                                       height-scale-factor)))
                        (reverse result))
              (let* ((row (vector-ref modules row-index))
                     (text-row
                      (let column-loop ((column-index 0)
                                        (text-row     ""))
                        (if (= column-index row-length)
                            text-row
                            (let* ((module (vector-ref row column-index))
                                   (text-module (make-string (* module-size
                                                                width-scale-factor)
                                                             (if module
                                                                 foreground-char
                                                                 background-char)))
                                   (text-module (if module
                                                    (if foreground-color
                                                        (string-append foreground-color
                                                                       text-module
                                                                       %no-color)
                                                        text-module)
                                                    (if background-color
                                                        (string-append background-color
                                                                       text-module
                                                                       %no-color)
                                                        text-module))))
                              (column-loop (+ column-index 1)
                                           (string-append text-row text-module)))))))
                (row-loop (+ row-index 1)
                          (cons text-row result)))))
        (let ((width (- (* size width-scale-factor)
                        (* margin 2))))
          (for-each (lambda _
                      (write-line (string-append
                                   background-color
                                   (make-string width background-char)
                                   %no-color)))
                    (iota (* margin height-scale-factor))))))))

(define* (qr-code->png-image qr-code
                             #:key
                             (size %default-qr-code-size)
                             (margin 0)
                             (foreground-color #vu8(0 0 0))
                             (background-color #vu8(255 255 255)))
  "Convert a @var{qr-code} to a PNG image.  This procedure requires Guile-PNG."
  (let ((draw!              (module-ref (resolve-interface '(png graphics))
                                        'draw!))
        (<png-image>        (module-ref (resolve-interface '(png image))
                                        '<png-image>))
        (<filled-rectangle> (module-ref (resolve-interface '(png graphics))
                                        '<filled-rectangle>))
        (<point>            (module-ref (resolve-interface '(png graphics))
                                        '<point>)))
    (unless draw!
      (error "Guile-PNG is not found"))
    (let* ((modules (QR-code-modules qr-code))
           (image (make <png-image>
                    #:color-type 2
                    #:bit-depth  8
                    #:width      (+ size (* margin 2))
                    #:height     (+ size (* margin 2))))
           (first-row (vector-ref modules 0))
           (rows-count (vector-length modules))
           (module-size (inexact->exact
                         (floor/ size
                                 (vector-length first-row)))))
      (draw! image
             (make <filled-rectangle>
               #:color    background-color
               #:position (make <point> #:x 0 #:y 0)
               #:height   (+ size (* margin 2))
               #:width    (+ size (* margin 2))))

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
                                          #:x (+ (* column-index module-size)
                                                 margin)
                                          #:y (+ (* row-index module-size)
                                                 margin))
                             #:width module-size
                             #:height module-size)))
                  (column-loop (+ column-index 1)))))
            (row-loop (+ row-index 1)))))
      image)))

;;; qr.scm ends here.
