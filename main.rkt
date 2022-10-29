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
      ; TODO: add options to decide debug llvm or not
      (command-line
       #:argv rest
       #:args (file-path)
       (compile-to-obj/exe file-path
                           #:debug-llvm? #t))])))

(module+ main
  (run-command))
