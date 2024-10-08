(define-module (qr-code core common)
  #:use-module (scheme documentation)
  #:export (%alphanumeric-charset
            object-address/hex-string
            constructor-argument))



(define %alphanumeric-charset
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:")



(define (object-address/hex-string object)
  (number->string (object-address object) 16))

(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))

;; common.scm ends here.

