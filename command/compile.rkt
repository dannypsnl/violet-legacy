#lang racket
(provide compile-to-obj/exe)
(require racket-llvm
         reporter)
(require violet/ast
         violet/parse
         violet/tyck
         violet/codegen)

(define (collect in)
  (define sexp-list '())
  (let loop ([r-sexp (read-syntax (object-name in) in)])
    (cond
      [(eof-object? r-sexp) (void)]
      [else (set! sexp-list (cons r-sexp sexp-list))
            (loop (read-syntax (object-name in) in))]))
  (reverse sexp-list))

(define (compile-to-obj/exe path
                            #:debug-llvm? [debug-llvm? #f])
  (define in (open-input-file path))
  (port-count-lines! in)
  (define sexp-list (collect in))
  (with-handlers ([Report? displayln])
    (define pmod (parse-file sexp-list))
    (cond
      [(stage0-app? pmod)
       ((compose (lambda (obj-path)
                   (system* (find-executable-path "clang") obj-path))
                 (lambda (bc-path)
                   (system* (find-executable-path "llc") bc-path "-filetype=obj")
                   (path-replace-extension bc-path ".o"))
                 (dump-llvm-mod path)
                 (if debug-llvm? print-llvm-mod identity)
                 codegen-app
                 type-check-app)
        pmod)]
      [(stage0-mod? pmod)
       ((compose (lambda (bc-path)
                   (system* (find-executable-path "llc") bc-path "-filetype=obj"))
                 (dump-llvm-mod path)
                 (if debug-llvm? print-llvm-mod identity)
                 codegen-mod
                 type-check-module)
        pmod)])))

(define (print-llvm-mod mod)
  (displayln (llvm-module->string mod))
  mod)

(define ((dump-llvm-mod path) mod)
  (define bc-path (path-replace-extension path #".bc"))
  (llvm-write-bitcode-to-file mod bc-path)
  bc-path)
