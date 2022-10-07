#lang racket
(provide parse-mod-file)
(require syntax/parse
         reporter)

(define (parse-mod-file sexp-list)
  (match-define (cons mod top-list) sexp-list)
  (syntax-parse mod
    #:datum-literals (module)
    [(module name:id)
     'mod]
    [else (raise
           (report
            #:error-code "E0001"
            #:message "invalid module syntax"
            #:target mod
            #:labels (list (label mod "here"
                                  #:color (color:red)))
            #:hint "help: read the formal description about the module syntax"))])
  )