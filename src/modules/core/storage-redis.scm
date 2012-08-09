(use redis-client)
(use s11n)

; Dummy config, until the real config module is built
(define *system-config*
  (make-parameter
    (alist->hash-table
      '(("db-host" . "localhost")
        ("db-port" . 6379)))))

(define *db-connected* (make-parameter #f))

(define (db-connect)
  (let* ((config (*system-config*))
         (host (hash-table-ref config "db-host"))
         (port (hash-table-ref config "db-port")))
    (redis-connect host port)
    (*db-connected* #t)))

(define (store-entity entity)
  (when (not (*db-connected*))
    (db-connect))
  (let ((store-simple
          (lambda (key val)
            (redis-hset key val)))
        (store-general
          (lambda (key val)
            (redis-hset key
                        (with-output-to-string
                          (lambda () (serialize val)))))))
    (hash-table-for-each entity
                         (lambda (key val)
                           (if (or (string=? key "#id") (string=? key "#type"))
                             (store-simple key val)
                             (store-general key val))))))

(define (retrieve-entity entity-id)
  (when (not (*db-connected*))
    (db-connect))
  (let ((entity (make-hash-table))
        (keys (redis-hkeys entity-id)))
    (for-each
      (lambda (k)
        (let ((value
                (if (or (string=? k "#id") (string=? k "#type"))
                  (redis-hget entity-id k)
                  (with-input-from-string (redis-hget entity-id k)
                                          (lambda ()
                                            (deserialize))))))
          (hash-table-set! entity k value)))
      keys)
    entity))
