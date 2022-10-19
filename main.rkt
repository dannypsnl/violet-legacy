#lang racket
(require "command.rkt"
         "tyck.rkt"
         "codegen.rkt")

(module+ main
  (define usecase (parse-command))
  (handle-command usecase))
