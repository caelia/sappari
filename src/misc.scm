(define (dir-fold f init start-dir)
  (let loop ((val init)
             (dir start-dir))
    (let* ((ff
             (directory dir))
           (files
             (map
               (lambda (fn)
                 (make-pathname dir fn))
               ff)))
      (foldl
        (lambda (v file)
          (when (directory? file)
            (loop v file))
          (f v file))
        val
        files))))

(define (dir-for-each f start-dir)
  (let loop ((dir start-dir))
    (let ((prev-dir (current-directory)))
      (change-directory dir)
      (let* ((files (directory))
             (result
               (for-each
                 (lambda (file)
                   (when (directory? file)
                     (loop file))
                   (f file))
                 files)))
        (change-directory prev-dir)
        result))))
