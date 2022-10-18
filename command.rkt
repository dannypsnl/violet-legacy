#lang racket
(require racket/cmdline)
(require "build.rkt")

(define command
  (command-line
    #:program "violet"
    #:args (subcommand . rest)
    (match subcommand
    ["build"
      (command-line
        #:argv rest
        #:args (dir-path)
        (cons subcommand dir-path)
      )])))

(match command
[(cons "build" dir-path)
  (build dir-path)])
