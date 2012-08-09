(module core
        ( @module-info
          new-eid
          config-get
          config-set!
          make-user
          verify-login
          store-entity
          retrieve-entity )

(import scheme)
(import chicken)
          
(define @module-info
  '(( "module-name" . "core" )
    ( "version" . "0.1" )
    ( "author" . "Matt Gushee <matt@gushee.net>" )
    ( "description" . "Provides essential functionality for all Sappari installations.")))

(include "util.scm")
(include "config.scm")
(include "auth.scm")
(include "storage-redis.scm")

)
