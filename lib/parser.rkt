#lang racket

; usage:
;   racket parser.rkt <ast-to-list.rkt> <program.txt>
; read <program.txt>, transrate it to s-exp and save as <program.sexp>

(require "parser0.rkt")

(define main
  (let*
    ([ast2list (vector-ref (current-command-line-arguments) 0)]
     [input-filename (vector-ref (current-command-line-arguments) 1)])
    (begin
      (parse ast2list input-filename))))
