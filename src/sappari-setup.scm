;;; sappari-setup.scm -- functions for creating a site and installing
;;;   optional components
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>.
;;;   * This is open source software, released under the BSD license.
;;;   See the LICENSE file for details.

(module sappari-setup 
        (setup-site-skeleton
         install-package)

        (import scheme)
        (import chicken)
        (import posix)
        (import files)

        (use z3)
        (use snowtar)
        (use easy-args)
        

;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; --  PUBLIC API  --------------------------------------------------------

(define (setup-site-skeleton #!key (parent-path ".") (site-dirname "sappari"))
  (let ((site-root (make-pathname parent-path site-dirname)))
    (create-directory site-root)))
  ;; Not done by a long shot


;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

)

;;; [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
;;; ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
;;; ------------------------------------------------------------------------
;;; ========================================================================
