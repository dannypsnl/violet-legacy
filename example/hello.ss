(module hello
  (export hello))

(: v : int64)
(define v 1)

(: hello : (int64 -> int64))
(define (hello a) a)

(: world : (int64 -> int64))
(define (world a)
  (+ a v))
