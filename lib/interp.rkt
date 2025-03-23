#lang racket

; usage:
;   racket interp.rkt <ast-to-list.rkt> <program.txt>
; read <program.txt>, transrate it to s-exp and save as <program.sexp>

(require "parser0.rkt")
(require "runner0.rkt")

(define interp
  (lambda
    (ast2list langfile programfile)
    (let ([s-exp-file (path-replace-extension programfile ".sexp")])
    (begin
;      (printf "s-exp-file:~s~%" (path->string s-exp-file))
      (parse ast2list programfile)
;      (printf "langfile:~s~%" langfile)
      (load-and-execute (path->string s-exp-file) langfile)))))

(define main
  (let* ([ast2list (vector-ref (current-command-line-arguments) 0)]
         [langfile (vector-ref (current-command-line-arguments) 1)]
         [programfile (vector-ref (current-command-line-arguments) 2)])
    (begin
;      (printf "ast2list:~s~%" ast2list)
;      (printf "langfile:~s~%" langfile)
;      (printf "programfile:~s~%" programfile)
      (interp ast2list langfile programfile))))
