#lang racket
(require redex)
(provide (all-defined-out))

; from the paper entitled "Run your research"

(define-language Lc  ; define Lc
  (e (e)
     (e e)       ; list of e's
     (e e e)
     x               ; non-terminal
;     (L (x) e)   ; L is terminal
;     call/cc         ; terminal
;     +               ; terminal
;     number)         ; built-in Racket number
     v)
  (v (L (x) e)   ; value
     call/cc
     +
     number)
  (x variable-not-otherwise-mentioned))

(define-extended-language
  Lcred Lc          ; define Lc/red using Lc
  (e .... (A e))     ; extends e, introduce A
;  (E (v ... E e ...) ; left-to-right
  (E (e ... E e ...) ; any-which-way
     hole))          ; hole in E

(define-extended-language
  any-which-way-Λc Lcred
  (E (e ... E e ...)
     hole))

(define red
  (reduction-relation
   Lcred #:domain e
   (--> (in-hole E (A e))
        e
        "abort")
   (--> (in-hole E (call/cc v))
        (in-hole E (v (L (x) (A (in-hole E x)))))
        (fresh x)
        "call/cc")
   (--> (in-hole E ((L (x) e) v))
        (in-hole E (subst e (x v)))
        "Betav")
   (--> (in-hole E (+ number ...))
        (in-hole E (sum number ...))
        "+")))

;(define any-which-way-red
;  (extend-reduction-relation
;   red any-which-way-Λc))

(define-metafunction Lcred
  subst : e (x v) ... -> e
  [(subst e (x_1 v_1) (x_2 v_2) ...)
   (subst-1 x_1 v_1 (subst e (x_2 v_2) ...))]
  [(subst e) e])

(define-metafunction Lcred
   no-occurrence : (x ...) x -> boolean
   [(no-occurrence (x_1 x_2 ...) x_3) 
    #f
    (side-condition (equal? (term x_1) (term x_3)))]
   [(no-occurrence (x_1 x_2 ...) x_3)
    (no-occurrence (x_2 ...) x_3)])

(define-metafunction Lcred
  subst-1 : x v e -> e
  [(subst-1 x_1 v x_1)
   v]
  [(subst-1 x v (e_1 e_2 e_3))
   ,(cons (term (subst-1 x v e_1))
         (term (subst-1 x v (e_2 e_3))))]
  [(subst-1 x v (e_1 e_2))
   ,(cons (term (subst-1 x v e_1))
         (term (subst-1 x v (e_2))))]
  [(subst-1 x v (e_1))
   ,(list (term (subst-1 x v e_1)))]
  [(subst-1 x v (L (x_1 ...) e))
   (L (x_1 ...) (subst-1 x v e))
   (side-condition (term (no-occurrence (x_1 ...) x)))]
  [(subst-1 x v (L (x_1 ...) e))
   (L (x_1 ...) e)]
  [(subst-1 x v (A e))
   (A (subst-1 x v e))]
  [(subst-1 x v +)
   +]
  [(subst-1 x v call/cc)
   call/cc]
  [(subst-1 x v number)
   number])

(define-metafunction Lcred
  sum : number ... -> number
  [(sum number ...)
   ,(foldr + 0 (term (number ...)))])

(define run
  (lambda (p)
    (apply-reduction-relation* red p)))
;(define run
;  (lambda (p)
;    (apply-reduction-relation* any-which-way-red p)))


(define p01 
    '2)

(define p02
    '((L (x) x) 3))

; (apply-reduction-relation red
;     (term 3))
; (apply-reduction-relation red
;     (term (+ 1 (A (+ 2 3)))))
;(traces any-which-way-red
;            (term (+ 1 (call/cc (λ (k) (+ (k 2) (k 3)))))))

;(apply-reduction-relation red (term ((L (x) x) 3)))
;(apply-reduction-relation red (term ((L (x) (+ x 2)) 3)))
;(traces red (term ((L (x) (+ x 2)) 3)))
;(traces red
;  (term ((L (x)
;            (+ ((L (x) (+ x 3)) 4) 5)) 6)))
; (run
;         (term (+ 1 (call/cc
;                     (L (k)
;                       (+ (k 2) (k 3)))))))
;(traces any-which-way-red
;        (term (+ 1 (call/cc
;                    (L (k)
;                      (+ (k 2) (k 3)))))))