#lang racket
(provide build)
(require "compile.rkt")

(define (build dir-path
               #:debug-llvm? [debug-llvm? #f])
  (find-files
    (lambda (path)
      (when [string-suffix? (path->string path) ".ss"]
        (touch (future (lambda ()
          (compile-to-obj/exe path #:debug-llvm? debug-llvm?))))))
    dir-path))
