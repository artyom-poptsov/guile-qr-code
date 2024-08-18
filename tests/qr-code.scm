(use-modules (srfi srfi-64)
             (ice-9 iconv)
             (qr-code))


(define %test-name "qr-code.scm")


(test-begin %test-name)

(test-assert "qr-encode-text"
  (qr-encode-text "hello world"))

(test-assert "qr-encode-text: ECC-HIGH"
  (qr-encode-text "hello world" #:ecl ECC-HIGH))

(test-assert "qr-encode-binary"
  (qr-encode-binary (string->bytevector "hello world" "UTF-8")))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
