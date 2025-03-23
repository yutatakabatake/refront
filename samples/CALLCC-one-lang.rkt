#lang racket
(require redex)
(provide (all-defined-out))
; ワンショットの継続

(define-language CALL/CC
  (v number
     boolean
     call/cc)
  (e v
     x
     (if e e e)
     (zero? e)
     (+ e e)
     (* e e)
     (let ((x e)) e)
     (lambda (x) e)
     (e e)     
     (letrec ((x e)) e))
  (x variable-not-otherwise-mentioned))

(define-extended-language Ev CALL/CC
  (P (e env))
  (E (if E e e)
     (zero? E)
     (+ number ... E e ...)
     (* number ... E e ...)
     (v ... E e ...)
     (let ((x E)) e)
     (E env)
     ;(letrec ((x E)) e)
     hole)
  (v ....
     (closure x e env))
  (e ....
     (A e))
  (env ((boolean x v) ...)))

(module+ test
  (test-equal (term (extend-env x 1 ((#f y 2)))) (term ((#f x 1) (#f y 2)))))

(define-metafunction Ev
  extend-env : x v env -> env
  [(extend-env x_0 v_0 ((boolean_1 x_1 v_1) ...))
   ((#f x_0 v_0) (boolean_1 x_1 v_1) ...)])

(module+ test
  (test-equal (term (lookup x ((#f x 1)))) 1)
  (test-equal (term (lookup x ((#f z 3) (#f y 2) (#f x 1)))) 1)

  )

(define-metafunction Ev
  lookup : x env -> v
  [(lookup x_0 ((#f x_0 v_0) (boolean x_1 v_1) ...)) v_0]

  [(lookup x_0 ((#t x_0 (closure x_arg e_pbody ((boolean_1 x_1 v_1) ...)))
                (boolean_2 x_2 v_2)))
   (closure x_arg e_pbody ((#t x_0 (closure x_arg e_pbody ((boolean_1 x_1 v_1) ...)))
                           (boolean_2 x_2 v_2)))]

  [(lookup x_0 ((#t x_1 (closure x_arg e_pbody ((boolean_2 x_2 v_2) ...)))
                (boolean_3 x_3 v_3)))
   (lookup x_0 ((boolean_2 x_2 v_2) ...))]

  [(lookup x_0 ((boolean_1 x_1 v_1) (boolean_2 x_2 v_2) ...))
   (lookup x_0 ((boolean_2 x_2 v_2) ...))]

  [(lookup x_0 ((boolean_1 x_1 v_1) ...))
   (error "No binding : " x_0)])

(define-metafunction Ev
  extend-env-rec : x v env -> env
  [(extend-env-rec x_0 (closure x_arg e_pbody env) ((boolean_1 x_1 v_1) ...))
   ((#t x_0 (closure x_arg e_pbody env)) (boolean_1 x_1 v_1) ...)])

(define red
  (reduction-relation
   Ev #:domain P
   (--> ((in-hole E x) env)
        ((in-hole E (lookup x env)) env)
        "var")
   (--> ((in-hole E (if #t e_1 e_2)) env)
        ((in-hole E e_1) env)
        "ift")
   (--> ((in-hole E (if #f e_1 e_2)) env)
        ((in-hole E e_2) env)
        "iff")
   (--> ((in-hole E (zero? e)) env)
        ((in-hole E (zero? e_prime)) env)
        (where e_prime (apply-reduction-relation (e env)))
        "zero?-prime")
   (--> ((in-hole E (zero? number)) env)
        ((in-hole E ,(zero? (term number))) env)
        "zero?")
   (--> ((in-hole E (+ number_1 number_2)) env)
        ((in-hole E ,(apply + (term (number_1 number_2)))) env)
        "+")
   (--> ((in-hole E (* number_1 number_2)) env)
        ((in-hole E ,(apply * (term (number_1 number_2)))) env)
        "*")
   (--> ((in-hole E (let ((x_1 e_1)) e_2)) env)
        ((in-hole E (let ((x_1 e_1prime)) e_2)) env)
        (where e_1prime (apply-reduction-relation red (e_1 env)))
        "let-rhs")
   (--> ((in-hole E (let ((x v)) e)) env)
        ((in-hole E e) (extend-env x v env))
        "let-body")
   (--> ((in-hole E (lambda (x) e)) env)
        ((in-hole E (closure x e env)) env)
        "lambda")
   (--> ((in-hole E ((closure x e_1 env_1) v)) env_2)
        ((in-hole E e_1) (extend-env x v env_1))
        "app")
   (--> ((in-hole E (call/cc v)) env)
        ((in-hole E (v (lambda (x) (A (in-hole E x))))) env)
        (fresh x)
        "call/cc")
   (--> ((in-hole E (A e)) env)
        (e env)
        "abort")
   (--> ((in-hole E (letrec ((x_pname (lambda (x_arg) e_pbody))) e_body)) env)
        ((in-hole E e_body) (extend-env-rec x_pname
                                            (closure x_arg e_pbody env)
                                            env))
        "letrec-body")
   ;;    (--> ((in-hole E (letrec ((x e_1)) e_2)) env)
   ;;         ((in-hole E (letrec ((x e_1prime) e_2))) env)
   ;;         (where e_1prime (apply-reduction-relation red (e_1 env)))
   ;;         "letrec-rhs")

   ))

(define init-env
  (term ((#f a 0))))

(define e1
  (term (+ 1 (call/cc (lambda (k) (k 2))))))

(define e2
  (term (letrec ((fact
                  (lambda (n)
                    (if (zero? n)
                        1
                        (* n (fact (+ n -1)))))))
          (fact 5))))

(define e3
  (term (letrec ((fact
                  (if (zero? 0)
                      (lambda (n)
                        (if (zero? n)
                            1
                            (* n (fact (+ n -1)))))
                      1)))
          (fact 5))))

(define e4
  (term (letrec ((double
                  (lambda (n)
                    (if (zero? n)
                        0
                        (+ (double (+ n -1)) 2)))))
          (double 9))))

(define e5
  (term (letrec ((fact
                  (lambda (x)
                    (if (zero? x)
                        (call/cc (lambda (k) (k 1)))
                        (* x (fact (+ x -1)))))))
          (fact 5))))

(define e? (redex-match? Ev e))

(module+ test
  (test-equal (e? e1) #t)
  (test-equal (e? e2) #t)
  (test-equal (e? e3) #t)
  (test-equal (e? e4) #t)
  (test-equal (e? e5) #t))

(module+ test
  (test-equal (run e1)
              (term 3))
  (test-equal (run e2)
              (term 120))
  (test-equal (run e4)
              (term 18))
  (test-equal (run e5)
              (term 120)))

(define run
  (lambda (pgm)
    (caar (apply-reduction-relation* red (term (,pgm ,init-env))))))
;    (car (apply-reduction-relation* red (term (,pgm ,init-env))))))

(define mytraces
  (lambda (pgm)
    (traces red (term (,pgm ,init-env)))))

(define mystepper
  (lambda (pgm)
    (stepper red (term (,pgm ,init-env)))))

(module+ test
  (test-results))
