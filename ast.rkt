#lang racket
(provide (struct-out stage0-mod))

(struct stage0-mod
  (name
   name=>type
   to-check-list)
  #:transparent)
