;;; sappari-setup.scm -- functions for creating a site and installing
;;;   optional components
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>.
;;;   * This is open source software, released under the BSD license.
;;;   See the LICENSE file for details.

(module sappari-setup 
        *
;        (setup-site-skeleton
;         install-package)

        (import scheme)
        (import chicken)
        (import posix)
        (import files)

        (use z3)
        (use snowtar)
        (use easy-args)
        

;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; ------------------------------------------------------------------------

(define +default-site-skeleton+
  '("build" "content" "etc" "extensions" ("sites" ("default")) "schemas"
          ("static" ("fonts" "pages" "scripts" "style" "media"))
          ("themes" (("plain" ("pink" "green"))))))


;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]



;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  PUBLIC API  --------------------------------------------------------

(define (setup-site-skeleton #!key (parent-path ".") (site-dirname "sappari"))
  (let ((site-root (make-pathname parent-path site-dirname)))
    (create-directory site-root)
    (change-directory site-root)
    (let loop ((dirs +default-site-skeleton+))
      (if (null? dirs)
        #t
        (let ((head (car dirs)))
          (if (string? head)
            (create-directory head)
            (let ((sub (car head)))
              (create-directory sub)
              (change-directory sub)
              (loop (cdr head))
              (change-directory "..")))
          (loop (cdr dirs)))))))


;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

)

;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
;;; ------------------------------------------------------------------------
;;; ========================================================================
