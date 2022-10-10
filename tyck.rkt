#lang racket
(provide type-check-module)
(require syntax/parse
         syntax/stx
         syntax/identifier
         reporter)
(require "ast.rkt")

(define (type-check-module s0mod)
  (match-define (stage0-mod name name=>type to-check-list) s0mod)
  (stage1-mod
   name
   (for/list ([top to-check-list])
     (syntax-parse top
       #:datum-literals (define)
       [(define name:id exp)
        (define expected-type (hash-ref name=>type (syntax->datum #'name) #f))
        (unless expected-type
          (raise (report #:error-code "E0003"
                         #:message "cannot find type"
                         #:target #'name
                         #:labels (list (label #'name "here" #:color 'red))
                         #:hint "help: please declare type for this definition")))
        ; TODO: check
        (def-var (identifier->string #'name)
          expected-type
          #'exp)]
       [(define (name:id p*:id ...) body ...+)
        (define expected-type (hash-ref name=>type (syntax->datum #'name) #f))
        (unless expected-type
          (raise (report #:error-code "E0003"
                         #:message "cannot find type"
                         #:target #'name
                         #:labels (list (label #'name "here" #:color 'red))
                         #:hint "help: please declare type for this definition")))
        ; TODO: check
        (def-func (identifier->string #'name)
          (stx-map identifier->string #'(p* ...))
          expected-type
          (syntax->list #'(body ...)))]))))
