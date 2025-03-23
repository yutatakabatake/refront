#lang racket
(require redex)
(provide (all-defined-out))

(define-language small
  (e ::= number
     (+ e e)))

(define-extended-language Ev small
  (P ::= e)
  (E ::= hole
     (+ number ... E e ...)))

(define red
  (reduction-relation Ev #:domain P
                      (--> (in-hole E (+ number_1 number_2))
                           (in-hole E ,(apply + (term (number_1 number_2))))
                           "+")))

(define run
  (lambda (pgm)
    (apply-reduction-relation* red (term ,pgm))))
