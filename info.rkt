#lang info
(define collection "violet")
(define deps '("base"
               "syntax-extension"
               "reporter"
               "racket-llvm"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "")
(define version "0.0")
(define license '(BSD3))
(define pkg-authors '(dannypsnl))

(define test-omit-paths '("example"))