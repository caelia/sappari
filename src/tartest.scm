(use snowtar)
(use srfi-4)

(define (test)
  (let* ((tr (tar-unpack-file "test.tar"))
         (cont (tar-rec-content (car tr))))
    (with-output-to-file
      "out.scm"
      (lambda ()
        (write-u8vector cont)))))
