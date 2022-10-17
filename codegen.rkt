#lang racket
(provide codegen-mod)
(require syntax/parse
         syntax/stx
         syntax/identifier
         reporter
         racket-llvm)
(require "ast.rkt")

(define (mangle-name mod-name name export-set)
  (if (set-member? export-set name)
      ; to export function needs a stable name
      (format "~a_~a" mod-name name)
      ; else it should not be stable
      (symbol->string (gensym name))))

(define (codegen-mod s1mod)
  (match-define (stage1-mod mod-name export-identifier-list to-compile-list) s1mod)
  (define export-set (map identifier->string export-identifier-list))

  (define mod (llvm-module mod-name))
  (define ctx (llvm-get-module-context mod))
  (define builder (llvm-builder-create))

  (define module-ctx
    (for/fold ([module-ctx '()])
              ([to-compile to-compile-list])
      (match to-compile
        [(def-var name ty exp)
         (define mangled-name (mangle-name mod-name name export-set))
         (define g (llvm-add-global mod
                                    (->llvmty ty)
                                    mangled-name))
         (llvm-set-initializer g (compile-expr builder (make-hash) exp))
         ; acc
         (cons (cons name
                     (list 'global (->llvmty ty) (llvm-get-named-global mod mangled-name)))
               module-ctx)]
        [else module-ctx])))

  (for ([to-compile to-compile-list])
    (match to-compile
      [(def-func name p-list ty body-list)
       (define f (llvm-add-function mod
                                    (mangle-name mod-name name export-set)
                                    (->llvmty ty)))
       (define local-ctx
         (for/list ([param-name p-list]
                    [i (length p-list)])
           ; map param to llvm expression via offset
           (cons param-name (llvm-get-param f i))))

       (define entry (llvm-append-basic-block f))
       ; shift builder insertion point to the end of the basic block
       (llvm-builder-position-at-end builder entry)

       (define return-value
         (last (for/list ([body body-list])
                 (compile-expr builder (append local-ctx module-ctx) body))))
       (llvm-build-ret builder return-value)]
      [else (void)]))

  mod)

(define (lookup builder ctx name)
  (define value (cdr (assoc name ctx)))
  (match value
    ; global variable need to load for computation, hence, stored in a verbose way
    [(list 'global ty v) (llvm-build-load2 builder ty v)]
    [v v]))

(define (compile-expr builder ctx exp)
  (syntax-parse exp
    #:datum-literals (+)
    [n:integer (llvm-const-int (llvm-int64-type) (syntax->datum #'n) #t)]
    [x:id (lookup builder ctx (identifier->string #'x))]
    [(+ e*:expr ...)
     (define compiled-e*
       (for/list ([e (syntax->list #'(e* ...))])
         (compile-expr builder ctx e)))
     (for/fold ([init-e (first compiled-e*)])
               ([e (rest compiled-e*)])
       (llvm-build-add builder init-e e))]
    [(f:expr e:expr ...)
     (raise (report #:error-code ""
                    #:message "general application not done yet"
                    #:target exp
                    #:labels (list (label exp "here" #:color 'red))
                    #:hint "TODO in compiler"))]
    [otherwise (raise (report #:error-code "E0005"
                              #:message "unknown expression"
                              #:target #'otherwise
                              #:labels (list (label #'otherwise "here" #:color 'red))
                              #:hint "the expression cannot be compiled"))]))

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
