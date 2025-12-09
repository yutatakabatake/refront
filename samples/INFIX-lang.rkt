#lang racket
(require redex)
(provide run)

(define-language INFIX
  (v number)
  (e v
     (l-brace e r-brace)
     ;  (fun x -> e)
     ;  (if e e e)
     ;  (e e)
     (e A e))
  (A +
     -
     *
     /))

(define INFIX?
  (lambda (x) (redex-match? INFIX e x)))

(define ex1
  (term (2 * (3 - 5))))

(define ex2
  (term (1 + (2 + (3 + 4)))))

(module+ test
  (test-equal (INFIX? ex1)
              #t)
  (test-equal (INFIX? ex2)
              #t))

(define-extended-language Ev INFIX
  (E hole
     ;  (E e)
     ;  (v E)
     (E A e)
     (v A E)
     (l-brace E r-brace)))

(define red
  (reduction-relation
   Ev #:domain e
   (--> (in-hole E (v_1 A v_2))
        (in-hole E (calc A v_1 v_2))
        "calc")
   (--> (in-hole E (l-brace e r-brace))
        (in-hole E (l-brace e_new r-brace))
        (where e_new (apply-reduction-relation red e))
        "brace")
   (--> (in-hole E (l-brace v r-brace))
        (in-hole E v)
        "v-brace")))

(define-metafunction Ev
  calc : A v v -> v
  [(calc + v_1 v_2)
   ,(apply + (term (v_1 v_2)))]
  [(calc - v_1 v_2)
   ,(apply - (term (v_1 v_2)))]
  [(calc * v_1 v_2)
   ,(apply * (term (v_1 v_2)))]
  [(calc / v_1 v_2)
   ,(apply / (term (v_1 v_2)))])

(define run
  (lambda (p)
    (apply-reduction-relation* red (term ,p))))

(module+ test
  (test-results))