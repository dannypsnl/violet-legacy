#lang racket
(require racket/cmdline)
(require "build.rkt"
         "compile.rkt")

(define command
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

(match command
[(cons "build" dir-path)
  (build dir-path)]
[(cons "compile" file-path)
  (compile file-path)])
