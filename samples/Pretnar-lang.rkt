#lang racket
(require redex)
(provide (all-defined-out))

(define-language Pretnar
  (v ::= x                ;value
     true
     false
     (fun x -> c)
     h)
  (h ::= (handler l-brace                                                  ;handler
                  return x_r -> c_r                                        ;return clause
                  (op-name (x semicolon k) -> c) ...
                  r-brace)) ;operation clauses
  (c ::= (return v)       ;computation ;return
     (op-name (v semicolon y dot c))         ;operation call
     (do x <- c_1 in c_2)    ;sequencing
     (if v then c_1 else c_2) ;conditional
     (apply v v)                ;application
     (with v handle c))   ;handling
  (x ::= variable-not-otherwise-mentioned)
  (op-name ::= variable-not-otherwise-mentioned)
  (y ::= variable-not-otherwise-mentioned)
  (k ::= variable-not-otherwise-mentioned)) ;continuation

(define-extended-language Pretnar-sugar Pretnar
  (v ::= ....
     (generic op-name))
  (c ::= ....
     (sequencing c_1 semicolon c_2)))

(define-extended-language PretnarEx Pretnar-sugar
  (v ::= ....
     integer
     string
     (+)
     (-)
     (*)
     (/)
     (<)
     (>)
     (<=)
     (>=)
     (=)
     (^)
     (rec fun x_fun-name x_bound c) ;recursive function
     unit
     (pair v_1 v_2)) ;pair
  #| (c ::= ....
        (do-pair p c1 in c2))
     (p ::= x
     (x x)) |#
  )

(define PretnarEx-c? (redex-match? PretnarEx c))
(define PretnarEx-v? (redex-match? PretnarEx v))
(define PretnarEx-h? (redex-match? PretnarEx h))

(module+ test
  (test-equal (PretnarEx-v? (term (pair 7 8))) #t)
  (test-equal (PretnarEx-v? (term (pair a b))) #t) ;変数と変数のpair
  (test-equal (PretnarEx-c? (term (apply a b))) #t) ;変数と変数のapplication
  (test-equal (PretnarEx-c? (term (return (pair 7 8)))) #t)
  (test-equal (PretnarEx-v? (term unit)) #t)
  (test-equal (PretnarEx-c? (term (op (4 semicolon y dot (return (pair x y)))))) #t)
  (test-equal (PretnarEx-v? (term (pair unit "a b c"))) #t)
  (test-equal (PretnarEx-v? (term (fun x -> (return (fun y -> (return (pair x y))))))) #t)
  (test-equal (PretnarEx-h? (term (handler l-brace
                                           return x -> (return x)
                                           (read (_ semicolon k) -> (apply k "Bob"))
                                           (print (s semicolon k) -> (apply k unit))
                                           r-brace))) #t) ;handler
  (test-equal (PretnarEx-c? (term (do _ <- (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) "What is your forename?") in
                                    (do forename <- (apply (fun x -> (read ( x semicolon y dot (return y) ))) unit) in (return forename))))) #t)
  (test-equal (PretnarEx-c? (term ,c1)) #t))

(define-extended-language Ev PretnarEx
  (p ::= c)
  (E ::= (do x <- E in c)
     (with h handle E)
     hole))


(module+ test
  (test-equal (term (subst ((x 1)) (return x)))
              (term (return 1)))
  (test-equal (term (subst ((x 1) (y 2)) (return (pair y (pair x (pair z x))))))
              (term (return (pair 2 (pair 1 (pair z 1))))))
  (test-equal (term (subst ((x 1)) (op ( 4 semicolon y dot (return (pair x y)) ))))
              (term (op ( 4 semicolon y dot (return (pair 1 y)) ))))
  (test-equal (term (subst ((x 1) (y 2)) (if true then (apply k x) else (apply (+) y))))
              (term (if true then (apply k 1) else (apply (+) 2))))
  (test-equal (term (subst ((_ unit)) (return _))) ;_も書き換えてしまう
              (term (return unit)))
  (test-equal (term (subst ((x 1) (y 2)) (return (pair x (pair y z)))))
              (term (return (pair 1 (pair 2 z))))))

(define-metafunction Ev
  subst : ((x v) ...) c -> c
  [(subst ((x_1 v_1) ...) (return v_2))
   (return (subst-raw ((x_1 v_1) ...) v_2))]

  [(subst ((x_1 v_1) ...) (op-name ( v_2 semicolon x_2 dot c )))
   (op-name ( (subst-raw ((x_1 v_1) ...) v_2) semicolon x_2 dot (subst ((x_1 v_1) ...) c) ))]

  [(subst ((x_1 v_1) ...) (do x_2 <- c_1 in c_2))
   (do x_2 <- (subst ((x_1 v_1) ...) c_1) in (subst ((x_1 v_1) ...) c_2))] ;;x_2だけ変更しない

  [(subst ((x_1 v_1) ...) (if v_2 then c_1 else c_2))
   (if (subst-raw ((x_1 v_1) ...) v_2) then (subst ((x_1 v_1) ...) c_1) else (subst ((x_1 v_1) ...) c_2))]

  [(subst ((x_1 v_1) ...) (apply v_2 v_3))
   (apply (subst-raw ((x_1 v_1) ...) v_2) (subst-raw ((x_1 v_1) ...) v_3))]

  [(subst ((x_1 v_1) ...) (with v_2 handle c))
   (with v_2 handle (subst ((x_1 v_1) ...) c))])

(define-metafunction Ev
  subst-raw : ((x v) ...) v -> v
  [(subst-raw ((x_1 v_1) (x_2 v_2) ...) x_1) v_1]

  [(subst-raw ((x_1 v_1) ...) (pair v_2 v_3))
   (pair (subst-raw ((x_1 v_1) ...) v_2) (subst-raw ((x_1 v_1) ...) v_3))]

;;   [(subst-raw ((x_1 v_1) ...) (fun x_f -> c_f))
;;    (fun x_f -> (subst ((x_1 v_1) ...) c_f))]

  [(subst-raw ((x_1 v_1) ...) (handler l-brace return x_r -> c_r (op-name_0 (x_0 semicolon k_0) -> c_0) ... r-brace))
   (handler l-brace return x_r -> c_r (op-name_0 (x_ semicolon0 k_0) -> (subst ((x_1 v_1) ...) c_0)) ... r-brace)]

  [(subst-raw ((x_1 v_1) (x_2 v_2) ...) x_0)
   (subst-raw ((x_2 v_2) ...) x_0)]

  [(subst-raw ((x_1 v_1) ...) any_1) any_1])

(module+ test
  (test-equal (term (handling (handler l-brace return x -> (return x)
                                       (read ( _ semicolon k ) -> (apply k "Bob"))
                                       (print ( s semicolon k ) -> (apply k unit)) r-brace)
                              (read ( unit semicolon y dot (do surename <- (return y) in
                                                             (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) (pair "Bob" surename))) ))))
              (term (apply (fun y -> (with (handler l-brace return x -> (return x)
                                                    (read ( _ semicolon k ) -> (apply k "Bob"))
                                                    (print ( s semicolon k ) -> (apply k unit)) r-brace)
                                           handle (do surename <- (return y) in
                                                    (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) (pair "Bob" surename)))))
                           "Bob")))
  (test-equal (term (handling
                     (handler l-brace return x -> (return x)
                              (read ( _ semicolon k ) -> (apply k "Bob"))
                              (print ( s semicolon k ) -> (apply k unit)) r-brace)
                     (print ( "What" semicolon y dot (return unit) ))))
              (term (apply (fun y -> (with (handler l-brace return x -> (return x)
                                                    (read ( _ semicolon k ) -> (apply k "Bob"))
                                                    (print ( s semicolon k ) -> (apply k unit)) r-brace)
                                           handle (return unit)))
                           unit))))

(define-metafunction Ev
  handling : h (op-name ( v semicolon y dot c )) -> c
  [(handling h (op-name_0 ( v semicolon y dot c_0 )))
   (handling-help h (op-name_0 ( v semicolon y dot c_0 )) ())])

(define-metafunction Ev
  handling-help : h (op-name ( v semicolon y dot c )) any -> c
  [(handling-help (handler l-brace return x_r -> c_r
                           (op-name_1 ( x_1 semicolon k_1 ) -> c_1)
                           (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace)
                  (op-name_1 ( v semicolon y dot c_0 )) ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ...))
   (subst ((x_1 v)
           (k_1 (fun y -> (with (make-handler ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ...)
                                              (handler l-brace return x_r -> c_r
                                                       (op-name_1 ( x_1 semicolon k_1 ) -> c_1)
                                                       (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace))
                                handle c_0))))
          c_1)]

  [(handling-help (handler l-brace return x_r -> c_r
                           (op-name_1 ( x_1 semicolon k_1 ) -> c_1)
                           (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace)
                  (op-name_0 ( v semicolon y dot c_0 ))
                  ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ...))
   (handling-help (handler l-brace return x_r -> c_r
                           (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace)
                  (op-name_0 ( v semicolon y dot c_0 ))
                  ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ... (op-name_1 ( x_1 semicolon k_1 ) -> c_1)))]

  [(handling-help (handler l-brace return x_r -> c_r
                           (op-name_1 ( x_1 semicolon k_1 ) -> c_1) ... r-brace)
                  (op-name_0 ( v semicolon y dot c_0 ))
                  ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ...))
   (op-name_0 ( v semicolon y dot
                  (with (make-handler ((op-name_3 ( x_3 semicolon k_3 ) -> c_3) ...)
                                      (handler l-brace return x_r -> c_r
                                               (op-name_1 ( x_1 semicolon k_1 ) -> c_1) ... r-brace))
                        handle c_0) ))])

(define-metafunction Ev
  make-handler : ((op-name ( x semicolon k ) -> c) ...) h -> h
  [(make-handler ((op-name_1 ( x_1 semicolon k_1 ) -> c_1) ...)
                 (handler l-brace return x_r -> c_r
                          (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace))
   (handler l-brace return x_r -> c_r
            (op-name_1 ( x_1 semicolon k_1 ) -> c_1) ...
            (op-name_2 ( x_2 semicolon k_2 ) -> c_2) ... r-brace)])

(module+ test
  (test-equal (term (add 6))
              (term (fun x -> (apply (+) (pair 6 x)))))
  (test-equal (term (add (pair 6 7)))
              (term 13)))

(define-metafunction Ev
  add : v -> v
  [(add number) (fun x -> (apply (+) (pair number x)))]

  [(add (pair number_1 number_2))
   ,(apply + (term (number_1 number_2)))])


(module+ test
  (test-equal (term (diff 6))
              (term (fun x -> (apply (-) (pair 6 x)))))
  (test-equal (term (diff (pair 6 7)))
              (term -1)))

(define-metafunction Ev
  diff : v -> v
  [(diff number) (fun x -> (apply (-) (pair number x)))]

  [(diff (pair number_1 number_2))
   ,(apply - (term (number_1 number_2)))])

(module+ test
  (test-equal (term (mult 6))
              (term (fun x -> (apply (*) (pair 6 x)))))
  (test-equal (term (mult (pair 6 7)))
              (term 42)))

(define-metafunction Ev
  mult : v -> v
  [(mult number) (fun x -> (apply (*) (pair number x)))]

  [(mult (pair number_1 number_2))
   ,(apply * (term (number_1 number_2)))])

(module+ test
  (test-equal (term (div 6))
              (term (fun x -> (apply (/) (pair 6 x)))))
  (test-equal (term (div (pair 6 2)))
              (term 3)))

(define-metafunction Ev
  div : v -> v
  [(div number) (fun x -> (apply (/) (pair number x)))]

  [(div (pair number_1 number_2))
   ,(apply / (term (number_1 number_2)))])

(module+ test
  (test-equal (term (greater 6))
              (term (fun x -> (apply (>) (pair 6 x)))))
  (test-equal (term (greater (pair 6 2)))
              (term true)))

(define-metafunction Ev
  greater : v -> v
  [(greater number) (fun x -> (apply (>) (pair number x)))]

  [(greater (pair number_1 number_2))
   ,(if (apply > (term (number_1 number_2))) (term true) (term false))])

(module+ test
  (test-equal (term (less 6))
              (term (fun x -> (apply (<) (pair 6 x)))))
  (test-equal (term (less (pair 6 2)))
              (term false)))

(define-metafunction Ev
  less : v -> v
  [(less number) (fun x -> (apply (<) (pair number x)))]

  [(less (pair number_1 number_2))
   ,(if (apply < (term (number_1 number_2))) (term true) (term false))])

(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole E (if true then c_1 else c_2))
        (in-hole E c_1)
        "ift")
   (--> (in-hole E (if false then c_1 else c_2))
        (in-hole E c_2)
        "iff")
   (--> (in-hole E (apply (fun x -> c) v))
        (in-hole E (subst ((x v)) c))
        "apply")
   (--> (in-hole E (do x <- (return v) in c))
        (in-hole E (subst ((x v)) c))
        "do-return")
   (--> (in-hole E (do x <- c_1 in c_2))
        (in-hole E (do x <- c_1prime in c_2))
        (where c_1prime (apply-reduction-relation red c_1))
        "do")
   (--> (in-hole E (do x <- (op-name ( v semicolon y dot c_1 )) in c_2))
        (in-hole E (op-name ( v semicolon y dot (do x <- c_1 in c_2) )))
        "do-op")
   (--> (in-hole E (with h handle c))
        (in-hole E (with h handle c_prime))
        (where c_prime (apply-reduction-relation red c))
        "handle")
   (--> (in-hole E (with (handler l-brace return x_r -> c_r
                                  (op-name_1 ( x_1 semicolon k_1 ) -> c_1) ... r-brace) handle (return v)))
        (in-hole E (subst ((x_r v)) c_r))
        "handle-return")
   (--> (in-hole E (with h handle (op-name_0 ( v semicolon y dot c_0 ))))
        (in-hole E (handling h (op-name_0 ( v semicolon y dot c_0 ))))
        "handle-operation")

   (--> (in-hole E (apply (+) v))
        (in-hole E (return (add v)))
        "+")
   (--> (in-hole E (apply (-) v))
        (in-hole E (return (diff v)))
        "-")
   (--> (in-hole E (apply (*) v))
        (in-hole E (return (mult v)))
        "*")
   (--> (in-hole E (apply (/) v))
        (in-hole E (return (div v)))
        "/")
   (--> (in-hole E (apply (>) v))
        (in-hole E (return (greater v)))
        ">")
   (--> (in-hole E (apply (<) v))
        (in-hole E (return (less v)))
        "<")

   ;; 画面にprintする
   (--> (in-hole E (apply display v))
        (in-hole E (out v))
        "display")

   ;  (--> (in-hole E (^ string_1 string_2))
   ;       (in-hole E ,(apply string-append (term (number_1 number_2))))
   ;       "string-append")
   ))


(module+ test
  (test-equal (term (out "Bob")) (term (return unit)))
  (test-equal (term (out "This is test.")) (term (return unit))))

(define-metafunction Ev
  out : v -> (return unit)
  [(out v)
   ,(begin
      (displayln (term v))
      (term (return unit)))])

;; sugar : generic effect
(module+ test
  (test-equal (term (generic print))
              (term (fun x -> (print ( x semicolon y dot (return y) )))))
  (test-equal (term (generic read))
              (term (fun x -> (read ( x semicolon y dot (return y) ))))))

(define-metafunction Ev
  generic : op-name -> v
  [(generic op-name)
   (fun x -> (op-name ( x semicolon y dot (return y) )))])

;; sugar : sequencing
(define-metafunction Ev
  sequencing : c_1 semicolon c_2 -> c_3
  [(sequencing c_1 semicolon c_2)
   (do ,(gensym) <- c_1 in c_2)])


;: readが3回でBobが7回
(define c-Bob-7times-sugar
  (term (with (handler l-brace
                       return x -> (return x)
                       (read ( _ semicolon k ) -> (do x <- (apply k "Bob") in (apply k x))) r-brace)
              handle (do forename <- (apply (generic read) unit) in
                       (do surename <- (apply (generic read) unit) in
                         (return (pair forename surename)))))))

(module+ test
  (test-->> red c-Bob-7times-sugar
            (term (return (pair (pair "Bob" (pair "Bob" "Bob")) (pair (pair "Bob" (pair "Bob" "Bob")) "Bob"))))))

;; 画面にprintするハンドラ
(define printer
  (term (handler l-brace
                 return x -> (return x)
                 (print ( s semicolon k ) -> (do x <- (apply display s) in (apply k unit))) r-brace)))

;; 実行して1回目しかprintされない?
;; 画面にprintする
(define c0
  (term (with ,printer handle
              (with (handler l-brace
                             return x -> (return x)
                             (read ( _ semicolon k ) -> (apply k "Bob")) r-brace)
                    handle (do _ <- (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) "What is your forename?") in
                             (do forename <- (apply (fun x -> (read ( x semicolon y dot (return y) ))) unit) in
                               (do _ <- (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) "What is your surname?") in
                                 (do surename <- (apply (fun x -> (read ( x semicolon y dot (return y) ))) unit) in
                                   (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) (pair #| join |# forename surename))))))))))

(define c0-sugar
  (term (with ,printer handle
              (with (handler l-brace
                             return x -> (return x)
                             (read ( _ semicolon k ) -> (apply k "Bob")) r-brace)
                    handle (sequencing (apply (generic print) "What is your forename?") semicolon
                                       (do forename <- (apply (generic read) unit) in
                                         (sequencing (apply (generic print) "What is your surename?") semicolon
                                                     (do surename <- (apply (generic read) unit) in
                                                       (apply (generic print) (pair forename surename))))))))))

;; 画面にprintしない
(define c1
  (term (with (handler l-brace
                       return x -> (return x)
                       (read ( _ semicolon k ) -> (apply k "Bob"))
                       (print ( s semicolon k ) -> (apply k unit)) r-brace)
              handle (do _ <- (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) "What is your forename?") in
                       (do forename <- (apply (fun x -> (read ( x semicolon y dot (return y) ))) unit) in
                         (do _ <- (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) "What is your surname?") in
                           (do surename <- (apply (fun x -> (read ( x semicolon y dot (return y) ))) unit) in
                             (apply (fun x -> (print ( x semicolon _ dot (return unit) ))) (pair #| join |# forename surename)))))))))

(define reverse
  (term (handler {l-brace return x -> (return x)
                          (print (s semicolon k) -> (do _ <- (apply k unit)
                                                      in (apply (fun x -> (print (x semicolon _ (return unit)))) s)))
                          r-brace})))

(define abc
  (term (do _ <- (apply (fun x -> (print (x semicolon _ (return unit)))) "A") in
          (do _ <- (apply (fun x -> (print (x semicolon _ (return unit)))) "B") in
            (apply (fun x -> (print (x semicolon _ (return unit)))) "C")))))

(define c2
  (term (with ,reverse handle ,abc)))

;; (module+ test
;;   (test-->> red c2 (term (print ("C" semicolon _ (do _ <- (do _ <- (return unit) in (apply (fun x -> (print (x semicolon _ (return unit)))) "B")) in (apply (fun x -> (print (x semicolon _ (return unit)))) "A")))))))

(define collect
  (term (handler {l-brace return x -> (return (pair x ""))
                          (print (s semicolon k) -> (do (pair-var x acc) <- (apply k unit)
                                                      in (return (pair x (pair #| join |# s acc)))))
                          r-brace})))

(define c3
  (term (with ,collect handle ,abc)))

;; (module+ test
;;   (test-->> red c3 (term (return (pair unit (pair "A" (pair "B" (pair "C" ""))))))))

(define c4
  (term (with ,collect handle (with ,reverse handle ,abc))))

;; (module+ test
;;   (test-->> red c4 (term (return (pair unit (pair "C" (pair "B" (pair "A" ""))))))))

(define collect-prime
  (term (handler {l-brace(return x -> (return (fun acc -> (return (pair x acc)))))
                         (print (s semicolon k) -> ((fun acc -> (do f <- (k unit) in (f x))) (pair acc s)))r-brace})))

(define default
  (term (fun x -> (return (handler {(return x -> (return x)) (raise (_ semicolon _) -> (return x))})))))

(define choose
  (term (fun (pair-var x y) -> (do b <- (apply (fun x -> (decide (x semicolon y (return y)))) unit)
                                 in (if b then (return x) else (return y))))))

(define pickTrue
  (term (handler {l-brace (return x -> (return x))
                          (decide (_ semicolon k) -> (apply k true))
                          r-brace})))

(define chooseDiff
  (term (do x1 <- (apply ,choose (pair 15 30)) in
          (do x2 <- (apply ,choose (pair 5 10)) in
            (do ans <- (apply (fun (pair-var x y) -> (do f <- (apply (-) x) in (apply f y))) (pair x1 x2))
              in (return ans))))))

(define c5
  (term (with ,pickTrue handle ,chooseDiff)))

;; (module+ test
;;   (test-->> red c5 (term (return 10))))

(define pickMax
  (term (handler {l-brace(return x -> (return x))
                         (decide (_ semicolon k) -> (do x_t <- (apply k true) in
                                                      (do x_f <- (apply k false) in
                                                        (do ans <- (apply (>) (pair x_t x_f)) in
                                                          (if ans then (return x_t) else (return x_f))))))
                         r-brace})))

(define c6
  (term (with ,pickMax handle ,chooseDiff)))

;; (module+ test
;;   (test-->> red c6 (term (return 25))))


;; (module+ test
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (+) x) in (apply f y))) (pair 7 8)) in (return ans))))
;;               (term ((return 15))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (-) x) in (apply f y))) (pair 7 8)) in (return ans))))
;;               (term ((return -1))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (*) x) in (apply f y))) (pair 7 8)) in (return ans))))
;;               (term ((return 56))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (/) x) in (apply f y))) (pair 20 4)) in (return ans))))
;;               (term ((return 5))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (<) x) in (apply f y))) (pair 20 4)) in (return ans))))
;;               (term ((return false))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair x y) -> (do f <- (apply (<) x) in (apply f y))) (pair 20 40)) in (return ans))))
;;               (term ((return true))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair-var x y) -> (do f <- (apply (>) x) in (apply f y))) (pair 20 4)) in (return ans))))
;;               (term ((return true))))
;;   (test-equal (run (term (do ans <- (apply (fun (pair-var x y) -> (do f <- (apply (>) x) in (apply f y))) (pair 20 40)) in (return ans))))
;;               (term ((return false)))))

(define run
  (lambda (p)
    (apply-reduction-relation* red p)))

(module+ test
  (test-results))