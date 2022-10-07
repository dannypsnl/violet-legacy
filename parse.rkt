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
            #:labels (list (label mod "here" #:color (color:red)))
            #:hint "help: read the formal description of the module syntax"))])

  (for ([top top-list])
    (syntax-parse top
      #:datum-literals (: define)
      [(: name:id : ty)
       'ty]
      [(define name:id exp)
       'def-var]
      [(define (name:id p*:id ...) exp)
       'def-proc]
      [else (raise
             (report
              #:error-code "E0002"
              #:message "invalid top level syntax"
              #:target top
              #:labels (list (label top "here" #:color (color:red)))
              #:hint "help: read the formal description of the top level syntax"))]))
  )