;;; sapparilib-utils.scm -- Miscellaneous useful functions to support
;;;   Sappari core.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.


(module sapparilib-utils
          *

          (import scheme)
          (import chicken)
          (import posix)
          (import files)
          (import ports)
          (import utils)
          (import data-structures)
          (import srfi-13)

          (use section-combinators)
          (use snowtar)
          (use z3)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  GENERIC DATA MANIPULATION  -----------------------------------------

;; A bottom-up version
(define (dir-fold-bu f init-val start-dir
                     #!optional (sort-fun (right-section sort string<)))
  (let loop ((val init-val)
             (files (list start-dir)))
    (if (null? files)
      val
      (let* ((file/dir (car files))
             (val*
               (if (directory? file/dir)
                 (loop
                   val
                   (map
                     (left-section make-pathname file/dir)
                     (sort-fun (directory file/dir))))
                 val)))
        (loop (f val* file/dir) (cdr files)))))) 

;; A top-down version
(define (dir-fold-td f init-val start-dir
                     #!optional (sort-fun (right-section sort string<)))
  (let loop ((val init-val)
             (files (list start-dir)))
    (if (null? files)
      val
      (let* ((file/dir (car files))
             (val*
               (f val file/dir))
             (val**
               (if (directory? file/dir)
                 (loop
                   val*
                   (map
                     (left-section make-pathname file/dir)
                     (sort-fun (directory file/dir))))
                 val*)))
        (loop val** (cdr files))))))

(define (dir-for-each f start-dir)
  (let loop ((dir start-dir))
    (let ((prev-dir (current-directory)))
      (change-directory dir)
      (let* ((files (directory))
             (result
               (for-each
                 (lambda (file)
                   (when (directory? file)
                     (loop file))
                   (f file))
                 files)))
        (change-directory prev-dir)
        result))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



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

(define (unpack-targz-archive arc-file
                              #!key (dest-dir #f)
                              (delete-archive #f))
  #f)

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------

