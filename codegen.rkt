#lang racket
(provide compile-mod)
(require syntax/parse
         syntax/stx
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
       (llvm-add-global mod
                        (->llvmty ty)
                        name)
       ; TODO: compile
       ]
      [(def-func name p-list ty body-list)
       (define f (llvm-add-function mod
                                    name
                                    (->llvmty ty)))
       (define entry (llvm-append-basic-block f))
       ; shift builder insertion point to the end of the basic block
       (llvm-builder-position-at-end builder entry)
       ; TODO: compile
       ]))

  (display (llvm-module->string mod))
  )
