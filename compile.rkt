#lang racket
(provide compile-to-object)

(require racket/runtime-path
         racket-llvm
         reporter)
(require "parse.rkt"
         "tyck.rkt"
         "codegen.rkt")

(define (collect in)
  (define sexp-list '())
  (let loop ([r-sexp (read-syntax (object-name in) in)])
    (cond
      [(eof-object? r-sexp) (void)]
      [else (set! sexp-list (cons r-sexp sexp-list))
            (loop (read-syntax (object-name in) in))]))
  (reverse sexp-list))

(define (compile-to-object path)
  (define in (open-input-file path))
  (port-count-lines! in)
  (define sexp-list (collect in))
  (with-handlers ([Report? displayln])
    ((compose (lambda (llvm-file-path)
                (system* (find-executable-path "llc") llvm-file-path "-filetype=obj"))
              (lambda (mod)
                (define llvm-file-path (path-replace-extension path #".bc"))
                (llvm-write-bitcode-to-file mod llvm-file-path)
                llvm-file-path)
              (lambda (mod)
                (display (llvm-module->string mod))
                mod)
              codegen-mod
              type-check-module
              parse-mod-file)
     sexp-list)))
