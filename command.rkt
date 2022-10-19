#lang racket
(provide parse-command
         handle-command)

(require racket/cmdline)
(require "build.rkt"
         "compile.rkt")

(define (parse-command)
  (define usecase
    (command-line
      #:program "violet"
      #:args (subcommand . rest)
      (match subcommand
      ["build"
        (command-line
          #:argv rest
          #:args (dir-path)
          (cons "build" dir-path)
        )]
      ["compile"
        (command-line
          #:argv rest
          #:args (file-path)
          (cons "compile" file-path))])))
    usecase)

(define (handle-command usecase)
  (match usecase
  [(cons "build" dir-path)
    (build dir-path)]
  [(cons "compile" file-path)
    (compile-to-object file-path)]))
