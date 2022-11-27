#lang racket
(provide build)
(require racket/async-channel)
(require "compile.rkt")

(define (build dir-path)
  (define paths (find-files
    (lambda (path) (path-has-extension? path #".ss"))
    dir-path))

  (define ch (make-async-channel))
  (define task-count (make-fsemaphore (length paths)))
  (define proc-count (make-fsemaphore (processor-count)))
  (for/async ([path paths])
    (fsemaphore-wait proc-count)
    (compile-to-obj/exe path #:debug-llvm? #f)
    (fsemaphore-wait task-count)
    (async-channel-put ch (fsemaphore-count task-count))
    (fsemaphore-post proc-count))
  (let loop ()
    (if (= 0 (async-channel-get ch))
      (void)
      (loop))))
