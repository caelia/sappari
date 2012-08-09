(use redis-client)
(use simple-sha1)
(use s11n)

(define *config*
  (make-parameter
    (alist->hash-table
      '(("db-host" . "localhost")
        ("db-port" . 6379)
        ("entity-space-magnitude" . 8)))))

(define *db-connected* (make-parameter #f))

(define (db-connect)
  (let* ((config (*config*))
         (host (hash-table-ref config "db-host"))
         (port (hash-table-ref config "db-port")))
    (redis-connect host port)
    (*db-connected* #t)))

(define (ensure-db-connection)
  (when (not (*db-connected*))
    (db-connect)))

(define (make-user uname password roles)
  (ensure-db-connection)
  (let ((user-id (string-append "/users/" uname)))
    (redis-hset user-id "uname" uname)
    (redis-hset user-id "passhash" (string->sha1sum password))
    (redis-hset user-id "roles"
                (with-output-to-string
                  (lambda () (serialize roles))))))

(define (verify-login uname password)
  (ensure-db-connection)
  (let ((user-id (string-append "/users/" uname))
        (passhash (string->sha1sum password)))
    (and (redis-exists user-id)
         (string=? (redis-hget user-id "passhash") passhash))))
