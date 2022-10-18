#lang racket
(provide build)

(define (build dir-path)
  (define files (filter
    (lambda file
      (string-suffix? (path->string (first file)) ".ss"))
    (directory-list dir-path)))
  (println files))
