#lang racket

; usage:
;   racket runner.rkt <langfile> <programfile>

(require "runner0.rkt")

(define main
 (let*
   ([langfile (vector-ref (current-command-line-arguments) 0)]
    [s-exp-file (vector-ref (current-command-line-arguments) 1)])
   (begin
    (load-and-execute s-exp-file langfile))))