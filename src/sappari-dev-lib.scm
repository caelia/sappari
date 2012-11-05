;;; sappari-tools.scm -- functions for setting up and maintaining
;;;   Sappari package directories.
;;; Copyright © 2012 by Matt Gushee <matt@gushee.net>.
;;; This is open source software, released under the BSD license.
;;;   See the LICENSE file for details.

(module sappari-tools 
        *
;        (
;         )

        (import scheme)
        (import chicken)
        (import posix)
        (import files)

        (use git)
        (use z3)
        (use snowtar)
        (use easy-args)
        

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GLOBAL DEFINITIONS  ------------------------------------------------

;; Recognized values are 'git, 'none, and 'manual
(define *version-control* (make-parameter 'git))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GIT INTERACTION  ---------------------------------------------------

;; Apparently there is no such predicate in the git egg, so we check for
;;   the existence of a .git directory.
(define (is-git-repo? #!optional (parent "."))
  (directory? (gitdir (make-pathname parent ".git"))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  PACKAGE DIRECTORIES  -----------------------------------------------

(define +default-site-skeleton+
  '("build" "content" "etc" "extensions" ("sites" ("demo")) "schemas"
          ("static" ("decor" "fonts" "media" "pages" "scripts" "style"))
          ("themes" (("plain" ("pink" "green"))))))

(define (extension-skeleton ext-name)
  (list (list "extensions" (list ext-name))))

(define (theme-skeleton theme-name #!optional subtheme-names)
  (if subtheme-names
    (list (list "themes" (list (list theme-name subtheme-names))))
    (list (list "themes" (list theme-name)))))

(define (schema-skeleton)
  (list "schemas"))

(define (ui-skeleton)
  (list "ui"))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
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
            (let ((head* (car head))
                  (subs (cadr head)))
              (create-directory head*)
              (change-directory head*)
              (loop subs)
              (change-directory "..")))
          (loop (cdr dirs)))))))


;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------
;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
;;; ========================================================================
;;; ------------------------------------------------------------------------

