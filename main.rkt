#lang racket
(require racket/cmdline)
(require "command/build.rkt"
         "command/compile.rkt")

(define debug-llvm? (make-parameter #f))

(define (run-command)
  (command-line
   #:program "violet"
   #:args (subcommand . rest)
   (match subcommand
     ["build"
      ; TODO: add options to decide debug llvm or not
      (command-line
       #:argv rest
       #:args (dir-path)
       (build dir-path
              #:debug-llvm? #t))]
     ["compile"
      (command-line
       #:argv rest
       #:once-any
       ["--debug-llvm" ("compile with llvm debug output")
                       (debug-llvm? #t)]
       #:args (file-path)
       (compile-to-obj/exe file-path
                           #:debug-llvm? (debug-llvm?)))])))

(module+ main
  (run-command))
