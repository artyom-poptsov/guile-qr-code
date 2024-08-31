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

(test-assert "qr-code->svg-image"
  (let ((qr-code (qr-encode-text "hello world")))
    (qr-code->svg-image qr-code)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
