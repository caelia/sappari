(use redis-client)
(use simple-sha1)
(use s11n)

(define *config*
  (make-parameter
    (alist->hash-table
      '(("db-host" . "localhost")
        ("db-port" . 6379)
        ("entity-space-magnitude" . 8))))

(define *db-connected* (make-parameter #f))

(define (db-connect)
  (let* ((config (*config*))
         (host (hash-table-ref config "db-host"))
         (port (hash-table-ref config "db-port")))
    (redis-connect host port)
    (*db-connected* #t)))

(define (new-eid)
  (when (not (*db-connected*))
    (db-connect))
  (if (redis-exists "#last-eid")
    (let ((eid (redis-get "#last-eid")))
      (redis-incr "#last-eid")
      eid))
    (let* ((start-magnitude
             (- (hash-table-ref (*config*) "entity-space-magnitude") 1))
           (result (expt 10 start-magnitude)))
      (redis-set "#last-eid" result)
      (number->string result)))

(define (make-user uname password roles)
  (when (not (*db-connected*))
    (db-connect))
  (let* ((eid (new-eid))
         (user-id (string-append "/users/" eid)))
    (redis-hset user-id "uname" uname)
    (redis-hset user-id "passhash" (string->sha1sum password))
    (redis-hset user-id "roles"
                (with-output-to-string
                  (lambda () (serialize roles))))))
