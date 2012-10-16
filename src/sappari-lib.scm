(use redis-client)
(use simple-sha1)
(use s11n)
(use json)


;;; ============================================================================
;;; --  GLOBAL PARAMETERS  -----------------------------------------------------

;;; The config object will become a hash table when loaded
(define *site-config* (make-parameter #f))

;;; This path is shared among all instances
(define *global-package-path* (make-parameter #f))

;;; Each instance sets this to its own location
(define *instance-package-path* (make-parameter #f))

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
  (ensure-config-loaded)
  (ensure-db-connection)
  (if (redis-exists "@last-eid")
    (let ((eid (redis-get "@last-eid")))
      (redis-incr "@last-eid")
      eid))
    (let* ((start-magnitude
             (- (hash-table-ref (*site-config*) "entity-space-magnitude") 1))
           (result (expt 10 start-magnitude)))
      (redis-set "@last-eid" result)
      (number->string result)))

(define (fun->string f)
  (with-output-to-string
    (lambda () (serialize f))))

(define (string->fun s)
  (with-input-from-string
    s
    (lambda () (deserialize))))

(define (->bool x)
  (case x
    ((0 "0" #\f #\F #\n #\N "f" "F" "n" "N") #f)
    ((1 "1" #\t #\T #\y #\Y "t" "T" "y" "Y") #t)
    (else (abort "Not a boolean value."))))

(define (bool->string b)
  (if b "1" "0"))

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


;;; ============================================================================



;;; ============================================================================
;;; --  BASE DATA MODEL  -------------------------------------------------------

(define (make-vocabulary name)
  (let ((name* (string-append "@vocab:" name)))
    (lambda (command . args)
      (case command
        ((set!)
         (when (redis-exists name*)
           (redis-del name*))
         (for-each
           (lambda (term) (redis-sadd name* term))
           args))
        ((add)
         (for-each
           (lambda (term) (redis-sadd name* term))
           args))
        ((get)
         (redis-smembers name*))
        ((del)
         (for-each
           (lambda (term) (redis-sdel name* term))))
        ((destroy)
         (redis-delete name*))))))

(define (make-data-type name validator scm->db db->scm)
  (ensure-db-connection)
  (let* ((validator* (fun->string validator))
         (scm->db* (fun->string scm->db))
         (db->scm* (fun->string db->scm))
         (key (string-append "@dt:" name)))
    (redis-watch key)
    (when (redis-exists key)
      (abort "Data type already exists."))
    (redis-multi)
    (redis-hset key "validator" validator*)
    (redis-hset key "scm->db" scm->db*)
    (redis-hset key "db->scm" db->scm*)
    (redis-exec)))

(define (make-structured-data-type name specs)
  (let ((structure-type (car specs)))
    (case structure-type
      ((list)
       (let ((fixed-length (bool->dbstring (cadr specs)))
             (homogeneous (bool->dbstring (caddr specs)))
             (element-types (cadddr specs)))
         (redis-watch name)
         (when (redis-exists name)
           (abort "Attempted to create a data type that already exists."))
         (redis-multi)
         (redis-hset name "structure-type" (symbol->dbstring structure-type))
         (redis-hset name "fixed-length" fixed-length)
         (redis-hset name "homogeneous" homogeneous)
         (if (string? element-types)
           (redis-hset name "element-types" (string->dbstring element-types))
           (let ((anon-id (make-anonymous-node)))
             (for-each
               (lambda (elt)
                 (redis-rpush anon-id (string->dbstring elt)))
               element-types)
             (redis-hset name "element-types" (string-append "I" anon-id))))))
      ((hash) #f)
      (else (abort "Not a valid structure type.")))))
    

(define (get-dtspec name #!optional fields)
  (let ((key (string-append "@dt:" name))
        (fields*
          (or fields
              '("validator" "scm->db" "db->scm"))))
    (map
      (lambda (field-name)
        (string->fun (redis-hget key field-name)))
      fields*)))

(define (make-schema node-type . field-specs)
  '())

(define (get-schema node-type)
  #f)

(define (make-node-proxy node-type)
  (let ((schema (get-schema node-type)))
    '()))

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
;;; --  INSTANCE MANAGEMENT  ---------------------------------------------------

(define (setup-instance name path #!optional (force #f))
  (let ((path*
          (if (absolute-pathname? path)
            path
            (normalize-pathname
              (make-pathname
                (current-directory) path)))))
   ))

;;; ============================================================================
