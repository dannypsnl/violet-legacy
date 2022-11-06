#lang racket
(provide build)
(require "compile.rkt")

(define (build dir-path
  #:debug-llvm? [debug-llvm? #f])
  ; FIXME: no need to build up a list, just directly compile them
  (define files
    (fold-files
     (lambda (path _ acc)
       (when [string-suffix? (path->string path) ".ss"]
        (compile-to-obj/exe path #:debug-llvm? debug-llvm?)))
     '() dir-path))
  (println files))
