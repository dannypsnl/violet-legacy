#lang racket
(provide type-check-app
         type-check-module)
(require syntax/parse
         syntax/stx
         syntax/identifier
         reporter)
(require "ast.rkt")

(define (type-check-app s0app)
  (match-define (stage0-app name import-list expr-list) s0app)
  (for/list ([expr expr-list])
    ; TODO: check expression like `apply`(function call) has correct typing
    (void))
  s0app)

(define (type-check-module s0mod)
  (match-define (stage0-mod name export-identifier-list name=>type to-check-list) s0mod)

  (hash-set! name=>type '+ #'(int64 int64 -> int64))

  (stage1-mod
   name
   export-identifier-list
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

        (check (hash->list name=>type) expected-type #'exp)

        (def-var (identifier->string #'name)
          expected-type
          #'exp)]
       [(define (name:id p*:id ...) body)
        (define expected-type (hash-ref name=>type (syntax->datum #'name) #f))
        (unless expected-type
          (raise (report #:error-code "E0003"
                         #:message "cannot find type"
                         #:target #'name
                         #:labels (list (label #'name "here" #:color 'red))
                         #:hint "help: please declare type for this definition")))

        (define params (stx-map identifier->symbol #'(p* ...)))
        (define params-type
          (syntax-parse expected-type
            [(param-ty* ... -> return-ty) (syntax->list #'(param-ty* ...))]
            [else (raise (report #:error-code "E0005"
                                 #:message "not a function type"
                                 #:target expected-type
                                 #:labels (list (label expected-type "here" #:color 'red))
                                 #:hint "help: please declare a correct function type"))]))

        (check (append (map cons
                            params
                            params-type)
                       (hash->list name=>type))
               (syntax-parse expected-type [(_ ... -> return-ty) #'return-ty])
               #'body)

        (def-func (identifier->string #'name)
          (stx-map identifier->string #'(p* ...))
          expected-type
          (list #'body))]))))

; (: lookup : Context Id -> Type)
(define (lookup ctx var)
  (dict-ref ctx (syntax->datum var)))

; (: synth : Context Expr -> Type)
(define (synth ctx exp)
  (syntax-parse exp
    [n:integer #'int64]
    [x:id (lookup ctx #'x)]
    [(f e ...)
     (syntax-parse (synth ctx #'f)
       [(arg-ty* ... -> return-ty)
        (for ([arg-ty (syntax->list #'(arg-ty* ...))]
              [e (syntax->list #'(e ...))])
          (check ctx arg-ty e))
        #'return-ty])]))

; (: type-equal? : Type Type -> boolean)
(define (type-equal? t1 t2)
  (eq? (syntax->datum t1) (syntax->datum t2)))

; (: check : Context Type Expr -> ?)
(define (check ctx ty exp)
  (unless (type-equal? ty (synth ctx exp))
    (raise (report #:error-code "E0004"
                   #:message "type mismatched"
                   #:target exp
                   #:labels (list (label ty (format "expected: ~a" (identifier->string ty)) #:color 'red)
                                  (label exp (format "actual: ~a" (identifier->string (synth ctx exp))) #:color 'red))
                   #:hint "help: "))))
