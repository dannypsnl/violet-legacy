#lang racket
(provide codegen-app
         codegen-mod)
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

(define (codegen-app s0app)
  (match-define (stage0-app app-name import-list expr-list) s0app)

  (define app-mod (llvm-module (format "~a_app" app-name)))
  (define ctx (llvm-get-module-context app-mod))
  (define builder (llvm-builder-create))

  (define main-fn (llvm-add-function app-mod "main"
                                     (llvm-function-type (llvm-int32-type))))
  (define entry (llvm-append-basic-block main-fn))
  ; shift builder insertion point to the end of the basic block
  (llvm-builder-position-at-end builder entry)

  (for/list ([expr expr-list])
    (compile-expr builder '() expr))

  (llvm-build-ret builder (llvm-const-int (llvm-int32-type) 0))

  app-mod)

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
  (define value? (assoc (identifier->string name) ctx))
  (unless value?
    (raise (report #:error-code "E0006"
                   #:message "cannot find variable"
                   #:target name
                   #:labels (list (label name "here" #:color 'red))
                   #:hint "help: please define this definition")))
  (define value (cdr value?))
  (match value
    ; global variable need to load for computation, hence, stored in a verbose way
    [(list 'global ty v) (llvm-build-load2 builder ty v)]
    [v v]))

(define (compile-expr builder ctx exp)
  (syntax-parse exp
    #:datum-literals (+)
    [n:integer (llvm-const-int (llvm-int64-type) (syntax->datum #'n) #t)]
    [x:id (lookup builder ctx #'x)]
    [(+ e*:expr ...)
     (define compiled-e*
       (for/list ([e (syntax->list #'(e* ...))])
         (compile-expr builder ctx e)))
     (for/fold ([init-e (first compiled-e*)])
               ([e (rest compiled-e*)])
       (llvm-build-add builder init-e e))]
    [(f:expr e:expr ...)
     (raise (report #:error-code "E0000"
                    #:message "general application not done yet"
                    #:target exp
                    #:labels (list (label exp "here" #:color 'red))
                    #:hint "help: TODO in compiler"))]
    [otherwise (raise (report #:error-code "E0005"
                              #:message "unknown expression"
                              #:target #'otherwise
                              #:labels (list (label #'otherwise "here" #:color 'red))
                              #:hint "the expression cannot be compiled"))]))

(define (->llvmty ty)
  (syntax-parse ty
    #:datum-literals (int64)
    [int8 (llvm-int8-type)]
    [int16 (llvm-int16-type)]
    [int32 (llvm-int32-type)]
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
