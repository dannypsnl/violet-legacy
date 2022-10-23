#lang racket
(provide run-command)

(require racket/cmdline)
(require "build.rkt"
         "compile.rkt")

(define (run-command)
  (command-line
    #:program "violet"
    #:args (subcommand . rest)
    (match subcommand
    ["build"
      (command-line
        #:argv rest
        #:args (dir-path)
        (build dir-path)
      )]
    ["compile"
      (command-line
        #:argv rest
        #:args (file-path)
        (compile-to-object file-path))])))
