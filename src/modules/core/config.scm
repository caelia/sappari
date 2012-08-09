(use json)

(define *config* (make-parameter #f))

(define (load-config)
  (let ((sappari-root (or (get-environment-variable "SAPPARI_ROOT") ".")))
    (*config*
      (alist->hash-table
        (vector->list
          (with-input-from-file (make-pathname (list sappari-root "config") "config.json")
                                (lambda () (json-read))))))))

(define (save-config)
  (let ((sappari-root (or (get-environment-variable "SAPPARI_ROOT") ".")))
    (with-output-to-file (make-pathname (list sappari-root "config") "config.json")
                         (lambda ()
                           (json-write
                             (list->vector
                               (hash-table->alist (*config*))))))))

(define (ensure-config-loaded)
  (when (not (*config*))
    (load-config)))

(define (config-get key)
  (ensure-config-loaded)
  (hash-table-ref (*config*) key))

(define (config-set! key value)
  (ensure-config-loaded)
  (hash-table-set! (*config*) key value)
  (save-config))
