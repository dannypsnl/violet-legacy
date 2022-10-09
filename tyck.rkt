#lang racket
(provide type-check-module)
(require syntax/parse
         reporter)
(require "ast.rkt")

(define (type-check-module s0mod)
  (match-define (stage0-mod _ name=>type to-check-list) s0mod)
  (for ([top to-check-list]
        #:when top)
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
       ]
      [(define (name:id p*:id ...) body ...+)
       (define expected-type (hash-ref name=>type (syntax->datum #'name) #f))
       (unless expected-type
         (raise (report #:error-code "E0003"
                        #:message "cannot find type"
                        #:target #'name
                        #:labels (list (label #'name "here" #:color 'red))
                        #:hint "help: please declare type for this definition")))
       ; TODO: check
       ]
      )
    (void)
    )
  )