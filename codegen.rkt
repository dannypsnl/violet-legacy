#lang racket
(provide compile-mod)
(require syntax/parse
         syntax/stx
         syntax/identifier
         reporter
         racket-llvm)
(require "ast.rkt")

(define (->llvmty ty)
  (syntax-parse ty
    #:datum-literals (int64)
    [int64 (llvm-int64-type)]
    [(t* ... -> t)
     (llvm-function-type
      (->llvmty #'t)
      (stx-map ->llvmty #'(t* ...)))]
    ; polymorphic type get rejected by frontend for now
    ; once it get enabled, functions can be separated to
    ; 1. we can compile now(mono type)
    ; 2. we have to wait invoking to the function(poly type)
    [otherwise (raise (report #:error-code "E0004"
                              #:message "not a mono-type"
                              #:target #'otherwise
                              #:labels (list (label #'otherwise "here" #:color 'red))))]))

(define (compile-mod s1mod)
  (match-define (stage1-mod name to-compile-list) s1mod)
  (define mod (llvm-module name))
  (define ctx (llvm-get-module-context mod))
  (define builder (llvm-builder-create))

  (for ([to-compile to-compile-list])
    (match to-compile
      [(def-var name ty exp)
       ; TODO: mangling
       (define g (llvm-add-global mod (->llvmty ty) name))
       (llvm-set-initializer g (compile-expr (make-hash) exp))]
      [(def-func name p-list ty body-list)
       ; TODO: mangling
       (define f (llvm-add-function mod name (->llvmty ty)))
       (define local-ctx (make-hash))
       (for ([p p-list]
             [i (length p-list)])
         (hash-set! local-ctx p (llvm-get-param f i)))

       (define entry (llvm-append-basic-block f))
       ; shift builder insertion point to the end of the basic block
       (llvm-builder-position-at-end builder entry)

       (define return-value
         (last (for/list ([body body-list])
                 (compile-expr local-ctx body))))
       (llvm-build-ret builder return-value)]))

  (display (llvm-module->string mod)))

(define (compile-expr local-ctx expr)
  (syntax-parse expr
    [n:integer (llvm-const-int (llvm-int64-type) (syntax->datum #'n) #t)]
    [x:id (hash-ref local-ctx (identifier->string #'x))]))
