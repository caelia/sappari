(use redis-client)
(use simple-sha1)
(use s11n)
(use json)


;;; ============================================================================
;;; --  GLOBAL PARAMETERS  -----------------------------------------------------

;;; The config object will become a hash table when loaded
(define *site-config* (make-parameter #f))

;;; The main dispatch table will hold functions provided by modules
(define *dispatch-table* (make-parameter (make-hash-table)))

;;; This table will hold all HTTP request handlers
(define *request-handlers* (make-parameter (make-hash-table)))

;;; Are we connected to Redis?
(define *db-connected* (make-parameter #f))

;;; ============================================================================



;;; ============================================================================
;;; --  CONFIG FILE PROCESSING  ------------------------------------------------

(define (load-config)
  (let ((sappari-root (or (get-environment-variable "SAPPARI_ROOT") ".")))
    (*site-config*
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
                               (hash-table->alist (*site-config*))))))))

(define (ensure-config-loaded)
  (when (not (*site-config*))
    (load-config)))

(define (config-get key)
  (ensure-config-loaded)
  (hash-table-ref (*site-config*) key))

(define (config-set! key value)
  (ensure-config-loaded)
  (hash-table-set! (*site-config*) key value)
  (save-config))

;;; ============================================================================



;;; ============================================================================
;;; --  REDIS DATABASE ACCESS  -------------------------------------------------

(define (db-connect)
  (ensure-config-loaded)
  (let* ((config (*site-config*))
         (host (hash-table-ref config "db-host"))
         (port (hash-table-ref config "db-port")))
    (redis-connect host port)))

(define (ensure-db-connection)
  (when (not (*db-connected*))
    (db-connect)))

(define (store-entity entity)
  (ensure-db-connection)
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
                           (if (or (string=? key "@id") (string=? key "@type"))
                             (store-simple key val)
                             (store-general key val))))))

(define (retrieve-entity entity-id)
  (ensure-db-connection)
  (let ((entity (make-hash-table))
        (keys (redis-hkeys entity-id)))
    (for-each
      (lambda (k)
        (let ((value
                (if (or (string=? k "@id") (string=? k "@type"))
                  (redis-hget entity-id k)
                  (with-input-from-string (redis-hget entity-id k)
                                          (lambda ()
                                            (deserialize))))))
          (hash-table-set! entity k value)))
      keys)
    entity))

;;; ============================================================================



;;; ============================================================================
;;; --  UTILITY FUNCTIONS  -----------------------------------------------------

(define (new-eid)
  (ensure-db-connection)
  (if (redis-exists "@last-eid")
    (let ((eid (redis-get "@last-eid")))
      (redis-incr "@last-eid")
      eid))
    (let* ((start-magnitude
             (- (hash-table-ref (*config*) "entity-space-magnitude") 1))
           (result (expt 10 start-magnitude)))
      (redis-set "@last-eid" result)
      (number->string result)))

;;; ============================================================================



;;; ============================================================================
;;; --  AUTHENTICATION AND SESSIONS  -------------------------------------------

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
    (if (and (redis-exists user-id)
             (string=? (redis-hget user-id "passhash") passhash))
      user-id
      #f)))

(define (make-session uname password)
  (let ((user-id (verify-login uname password)))
    (and user-id
         (let* ((session-no (new-eid))
                (session-key (string-append "/sessions/" session-no)))
           (if (redis-exists session-key)
             #f
             (begin
               (ensure-config-loaded)
               (let ((timeout (config-get "session-timeout")))
                 (redis-multi)
                 (redis-set session-key user-id)
                 (redis-expire session-key timeout)
                 (redis-exec))
               session-key))))))

(define (refresh-session session-key)
  (ensure-config-loaded)
  (let ((timeout (config-get "session-timeout")))
    (and (redis-exists session-key)
         (redis-expire session-key))))

(define (end-session session-key)
  (redis-del session-key))
       (redis-expire session-key


;;; ============================================================================



;;; ============================================================================
;;; --  BASE DATA MODEL  -------------------------------------------------------
;;; ============================================================================



;;; ============================================================================
;;; --  PACKAGE MANAGEMENT API  ------------------------------------------------

(define (module? path)
  #f)
(define (theme? path)
  #f)
(define (unarchive-package path)
  #f) 
(define (verify-package path) 
  #f)
(define (build-module path)
  #f)
(define (install-module src dest)
  #f)
(define (install-theme src dest)
  #f)

;;; ============================================================================




;;; ============================================================================
;;; ----------------------------------------------------------------------------
;;; ============================================================================
