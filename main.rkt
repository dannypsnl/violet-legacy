#lang racket
(require racket/runtime-path
         reporter)
(require "parse.rkt"
         "tyck.rkt")

(define-runtime-path test-example "example/hello.ss")

(define (collect in)
  (define sexp-list '())
  (let loop ([r-sexp (read-syntax (object-name in) in)])
    (cond
      [(eof-object? r-sexp) (void)]
      [else (set! sexp-list (cons r-sexp sexp-list))
            (loop (read-syntax (object-name in) in))]))
  (reverse sexp-list))

(module+ main
  (define in (open-input-file test-example))
  (port-count-lines! in)
  (define sexp-list (collect in))
  (with-handlers ([Report? displayln])
    ((compose type-check-module
              parse-mod-file) sexp-list)))
