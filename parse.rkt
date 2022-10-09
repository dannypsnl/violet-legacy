#lang racket
(provide parse-mod-file)
(require syntax/parse
         reporter)
(require "ast.rkt")

(define-syntax-class type
  #:literals (->)
  (pattern name:id)
  (pattern (name:id ty:type ...))
  (pattern (t*:type -> t:type)))

(define (parse-mod-file sexp-list)
  (match-define (cons mod top-list) sexp-list)
  (define name #f)
  (syntax-parse mod
    #:datum-literals (module)
    [(module n:id)
     (set! name (symbol->string (syntax->datum #'n)))]
    [else (raise (report
                  #:error-code "E0001"
                  #:message "invalid module syntax"
                  #:target mod
                  #:labels (list (label mod "here" #:color 'red))
                  #:hint "help: read the formal description of the module syntax"))])

  (define name=>type (make-hash))
  (define to-check-list
    (for/list ([top top-list])
      (syntax-parse top
        #:datum-literals (: define)
        [(: name:id : ty:type)
         (hash-set! name=>type
                    (syntax->datum #'name)
                    (syntax->datum #'ty))
         #f]
        [(define name:id exp) top]
        [(define (name:id p*:id ...) body ...+) top]
        [else (raise (report
                      #:error-code "E0002"
                      #:message "invalid top level syntax"
                      #:target top
                      #:labels (list (label top "here" #:color 'red))
                      #:hint "help: read the formal description of the top level syntax"))])))
  (stage0-mod name name=>type to-check-list))
