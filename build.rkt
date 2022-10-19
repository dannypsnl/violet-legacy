#lang racket
(provide build)

(define (build dir-path)
  (define files
    (fold-files
      (lambda (path _ acc)
        (if [string-suffix? (path->string path) ".ss"] (cons path acc) acc))
      '() dir-path))
  (println files))
