#lang racket
(require racket/cmdline)
(require "command/build.rkt"
         "command/compile.rkt")

(define (run-command)
  (command-line
   #:program "violet"
   #:args (subcommand . rest)
   (match subcommand
     ["build"
      (command-line
       #:argv rest
       #:args (dir-path)
       (build dir-path))]
     ["compile"
      (command-line
       #:argv rest
       #:args (file-path)
       (compile-to-obj/exe file-path))])))

(module+ main
  (run-command))
