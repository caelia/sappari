;;; sappari-tools.scm -- Support library for command-line tools.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.


(module sappari-tools
          *

          (import scheme)
          (import chicken)
          (import posix)
          (import files)
          (import ports)
          (import utils)
          (import data-structures)
          (import srfi-4)
          (import srfi-13)

          (use section-combinators)
          (use git)
          (use snowtar)
          (use z3)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  TAR ARCHIVE HANDLING  ----------------------------------------------

(define (directory->tar-data dir)
  (with-output-to-string
    (lambda ()
      (tar-pack-genport (tar-read-file dir) (current-output-port)))))


(define (directory->tar-file dir #!key (tar-name #f))
  (let ((tar-file (or tar-name (string-append dir ".tar"))))
    (tar-pack-file (tar-read-file dir) tar-file)
    tar-file))

(define (directory->targz-data dir)
  #f)

(define (directory->targz-file dir
                               #!key (targz-name #f)
                               (dest-dir #f)
                               (delete-src #f))
  (let* ((targz-file (or targz-name (string-append dir ".tar.gz")))
         (fno (file-open targz-file (+ open/wronly open/creat)))
         (zfh (z3:encode-file fno))
         (data (directory->tar-data dir)))
    (z3:write-encoded zfh data)
    (z3:write-encoded zfh #f)
    (file-close fno)))

(define (gzfile->data file-name)
  (let ((zport (z3:open-compressed-input-file file-name)))
    (with-output-to-string
      (lambda ()
        (let ((result (read-all zport)))
          (close-input-port zport)
          (display result))))))

(define (unpack-targz-archive arc-file
                              #!key (dest-dir #f)
                              (delete-archive #f))
  (let* ((pfxer
           (if dest-dir
             (left-section make-pathname dest-dir)
             identity))
         (tar-string
           (gzfile->data arc-file))
         (tar-recs
           (with-input-from-string tar-string
             (lambda ()
               (tar-unpack-genport (current-input-port))))))
    (for-each
      (lambda (trec)
        (let ((path (pfxer (tar-rec-name trec))))
          (if (eqv? (tar-rec-type trec) 'directory)
            (create-directory path #t)
            (with-output-to-file
              path
              (lambda ()
                (write-u8vector (tar-rec-content trec)))))))
      tar-recs)))


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

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

