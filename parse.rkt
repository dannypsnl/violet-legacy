#lang racket
(provide parse-file)
(require syntax/parse
         reporter)
(require "ast.rkt")

(define-syntax-class type
  #:literals (->)
  (pattern name:id)
  (pattern (name:id ty:type ...))
  (pattern (t*:type -> t:type)))

(define (parse-file sexp-list)
  (match-define (cons header top-list) sexp-list)
  (syntax-parse header
    #:datum-literals (app module export import)
    [(module n:id
       (~optional (export export-ids:id ...)))
     (define name ((compose symbol->string syntax->datum) #'n))
     (define export-identifier-list
       (if (attribute export-ids)
           (syntax->list #'(export-ids ...))
           '()))

     (define name=>type (make-hash))
     (define to-check-list
       (for/list ([top top-list])
         (syntax-parse top
           #:datum-literals (: define)
           [(: name:id : ty:type)
            (hash-set! name=>type (syntax->datum #'name) #'ty)
            #f]
           [(define name:id exp) top]
           [(define (name:id p*:id ...) body ...+) top]
           [else (raise (report
                         #:error-code "E0002"
                         #:message "invalid top level syntax"
                         #:target top
                         #:labels (list (label top "here" #:color 'red))
                         #:hint "help: read the formal description of the top level syntax"))])))
     (stage0-mod name export-identifier-list name=>type (filter-map identity to-check-list))]
    [(app n:id
          (~optional (import import-ids:id ...)))
     (define name ((compose symbol->string syntax->datum) #'n))
     (define import-identifier-list
       (if (attribute import-ids)
           (syntax->list #'(import-ids ...))
           '()))
     (stage0-app name
                 import-identifier-list
                 top-list)]
    [else (raise (report
                  #:error-code "E0001"
                  #:message "invalid module/app syntax"
                  #:target header
                  #:labels (list (label header "here" #:color 'red))
                  #:hint "help: read the formal description of the module/app syntax"))]))
