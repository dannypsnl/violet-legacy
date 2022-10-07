#lang racket
(require racket/runtime-path)

(define-runtime-path test-example "example/hello.ss")

(define (collect in)
  (let loop ([r-sexp (read-syntax (object-name in) in)])
    (cond
      [(eof-object? r-sexp) (void)]
      [else (println r-sexp)
            (loop (read-syntax (object-name in) in))]))
  )

(module+ main
  (define in (open-input-file test-example))
  (collect in)
  )