#lang racket
(provide parse)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-empty-tokens _control (EOF))
(define-empty-tokens _keyword
  (MODULE
   OPEN
   LET
   FUN))
(define-empty-tokens _symbol
  (:
   =
   -> =>
   +
   OPEN-PAREN CLOSE-PAREN))
(define-tokens _integer (INTEGER))
(define-tokens _identifier (ID))

(define (violet/tokenize in)
  (port-count-lines! in)
  (define violet/lexer
    (lexer-src-pos
     [(eof) (token-EOF)]
     [":" (token-:)]
     ["=" (token-=)]
     ["->" (token-->)]
     ["=>" (token-=>)]
     ["+" (token-+)]
     ["(" (token-OPEN-PAREN)]
     [")" (token-CLOSE-PAREN)]
     ["module" (token-MODULE)]
     ["open" (token-OPEN)]
     ["let" (token-LET)]
     ["fun" (token-FUN)]
     ;; integer
     [(:+ numeric) (token-INTEGER (string->number lexeme))]
     ;; identifier
     [(:: (:or alphabetic #\_)
          (:* (:or alphabetic numeric #\_ #\-)))
      (token-ID lexeme)]
     ;; comment
     [(:: "--" (:* (:~ #\newline)) #\newline)
      (return-without-pos (violet/lexer input-port))]
     [(:or whitespace blank)
      (return-without-pos (violet/lexer input-port))]))
  (λ () (violet/lexer in)))

(define violet/parser
  (parser
   (src-pos)
   (tokens _control _keyword _symbol _integer _identifier)
   (expected-SR-conflicts 3)
   (start mod) (end EOF)
   (error
    (λ (tok-ok? tok-name tok-value start-pos end-pos)
      (println (list "error: " tok-ok? tok-name tok-value start-pos end-pos))
      (void)))
   (grammar
    (mod [(MODULE ID definitions)
          (list $2 $3)])
    (definition
      [(LET ID : type = expr)
       (list "let" $2 ":" $4 "=" $6)]
      [(LET ID = expr)
       (list "let" $2 "=" $4)])
    (definitions [() '()] [(definition definitions) (cons $1 $2)])
    (type
     [(type -> type) (list $1 "->" $3)]
     [(ID) $1])
    (expr [(OPEN-PAREN exprs CLOSE-PAREN) $2]
          [(FUN ID => expr) (list "fun" $2 "=>" $4)]
          [(expr + expr) (list $1 "+" $3)]
          [(INTEGER) $1]
          [(ID) $1])
    (exprs [() '()] [(expr exprs) (cons $1 $2)]))))

(define (parse in)
  (violet/parser (violet/tokenize in)))
