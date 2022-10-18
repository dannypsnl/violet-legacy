#lang racket
(require racket/cmdline)

(define violet
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

(println violet)
