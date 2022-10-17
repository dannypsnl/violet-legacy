#lang racket
(provide (struct-out stage0-mod)
         (struct-out stage1-mod)
         (struct-out def-var)
         (struct-out def-func))

(struct stage0-mod
  (name ; (module name)
   export-identifier-list ; (export id ...)
   name=>type ; internal: map from name to type
   to-check-list)
  #:transparent)

(struct stage1-mod
  (name ; (module name)
   export-identifier-list ; (export id ...)
   to-compile-list)
  #:transparent)

(struct def-var (name mono-type expr))
(struct def-func (name
                  param-list
                  mono-type
                  body-list))
