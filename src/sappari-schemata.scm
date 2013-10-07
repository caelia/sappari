;;; sappari-schemata.scm -- Tools for reading & writing schema files.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.

(module sappari-schemata
        *
        (import scheme)
        (import chicken)

        (use ssax)


(define directory-layout-schema-schema
  '(:! "schema"
       (:+ "directory"
           (:@ "name" #f)
           (:&))))

(define (validate-schema schema-file schema-schema)
  #f)

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

