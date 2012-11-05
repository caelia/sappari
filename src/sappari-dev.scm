;;; sappari-dev.scm -- program to help developers set up & maintain
;;;   Sappari package directories.
;;;   Copyright © 2012 by Matt Gushee <matt@gushee.net>.
;;;   * This is open source software, released under the BSD license.
;;;   See the LICENSE file for details.

(module sappari-setup 
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
;;; ------------------------------------------------------------------------

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
;;; ========================================================================

(define pkginfo-template)

(define api-template)

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  MAIN PROGRAM  ------------------------------------------------------

(define (setup-project-dir args)
  (let ((parent-opt (args:make-option (P parent) #:required ""))
        (plugin-opt (args:make-option (p plugin) #:none ""))
        (theme-opt (args:make-option (t theme) #:none ""))
        (schema-opt (args:make-option (s schema) #:none ""))
        (ui-opt (args:make-option (u user-interface) #:none "")))
  #f)

(define (run-tests args)
  #f)

(define (generate-docs args)
  #f)

(define (verify-package args)
  #f)

(define (create-package args)
  #f)

(define (run)
  (let ((cmd (string->symbol (cadr (argv))))
        (args (cddr (argv))))
    (case cmd
      ((setup)    (setup-project-dir args))
      ((test)     (run-tests args))
      ((docs)     (generate-docs args))
      ((verify)   (verify-package args))
      ((package)  (create-package args))
      (else       (error "Unknown command")))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
