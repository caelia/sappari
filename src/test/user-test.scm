;;; Tests schema creation and node creation capabilities. Will probably become part of the production codebase

(make-data-type "string" string? (lambda (s) (string-append "s" s)) identity)

(let ((vocab (make-vocabulary "system-role")))
  (vocab 'set! "administrator" "editor" "member" "guest"))

(make-structured-data-type "system-role-list"
                           '(list #f #t "system-role"))


(make-schema "user"
             '(("username" "string" #t)
               ("first-name" "string" #f)
               ("middle-name" "string" #f)
               ("last-name" "string" #f)
               ("email" "string" #t)
               ("password" "string" #t)   ; This is a case where we need to distinguish the data entered by
                                          ; the user from the stored data. Also probably need regex validation.
               ("roles" "system-role-list" '(#t '()))))
