#lang racket
(provide build)

(define (build dir-path)
  ; FIXME: no need to build up a list, just directly compile them
  (define files
    (fold-files
     (lambda (path _ acc)
       (if [string-suffix? (path->string path) ".ss"] (cons path acc) acc))
     '() dir-path))
  (println files))
