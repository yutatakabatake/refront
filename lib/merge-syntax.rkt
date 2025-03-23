#lang racket
(require "common.rkt")

;; 拡張された情報をまとめる
;; 拡張の最後の情報が評価文脈の定義である前提

; 拡張情報をリストに格納
(define extended-def
  (lambda (fin)
    (letrec ((collect
              (lambda (data out-list)
                (cond
                  ((eof-object? data)
                   (begin
                     (close-input-port fin)
                     ;  最後のreduction-relationの定義は捨てる
                     (reverse (cdr (reverse out-list)))))
                  (else
                   (collect (read fin) (append out-list (list data))))))))
      (collect (read fin) '()))))

(define merge
  (lambda (first-lang extend-lang)
    (let ((def (car first-lang))
          (lang-name (cadr first-lang)))
      (cond
        ((null? extend-lang)
         first-lang)
        (else
         (merge (append (list def)
                        (list lang-name)
                        (merge-productions
                         (cddr first-lang)
                         (cdddr (car extend-lang))
                         '()))
                (cdr extend-lang)))))))

(define merge-productions
  (lambda (first-productions extend-productions result)
    (cond
      ; 新しい非終端記号を結合
      ((null? first-productions)
       (append result extend-productions))
      ;  すでに定義されている非終端記号を結合
      (else
       (let ((merged (car (merge-production (car first-productions) extend-productions '())))
             (rest-extends (cadr (merge-production (car first-productions) extend-productions '()))))
         (merge-productions
          (cdr first-productions)
          rest-extends
          (append result (list merged))))))))

; first-production : '(v x boolean (fun x -> c) h)
; extend-productions : '((v .... (generic op-name)) (c .... (sequencing c_1 semicolon c_2)) (binop + - * / == < > != <= >=))
; 第1要素が結合された生成規則
; 第2要素が結合されなかった生成規則のリスト
(define merge-production
  (lambda (first-production extend-productions rest-extend-productions)
    (let ((first-sym (car first-production)))
      (cond
        ((null? extend-productions)
         (list first-production rest-extend-productions))
        (else
         (let* ((extend-production (car extend-productions))
                (extend-sym (car extend-production)))
           (cond
             ; すでに定義している非終端記号を拡張
             ((equal? first-sym extend-sym)
              (list
               (append first-production (cddr extend-production))
               (append rest-extend-productions (cdr extend-productions))))
             (else
              (merge-production
               first-production
               (cdr extend-productions)
               (append rest-extend-productions (list extend-production)))))))))))

; 言語設計者から開始記号の指示を受け取る
(define instruction-start-symbol
  (lambda (domain merged-lang)
    (begin
      (let ([candidates (cdr (assoc domain (cddr merged-lang)))]) ;右辺だけを取り出す
        (display "candidates : ")
        (displayln candidates)
        (display "Enter start symbol : ")
        (let ([start-symbol (read)])
          start-symbol)))))

(define main
  (lambda ()
    (let* ((input-file (vector-ref (current-command-line-arguments) 0))
           (output-file (vector-ref (current-command-line-arguments) 1))
           (fin (open-input-file input-file))
           (reductino-relation-fin (open-input-file input-file))
           (fout (open-output-file output-file #:exists 'replace))
           (first-def (read fin))
           (merged-lang (merge first-def (extended-def fin)))
           (reduction-relation-data (get-last-data reductino-relation-fin))
           (domain (get-domain reduction-relation-data))
           (start-symbol (instruction-start-symbol domain merged-lang))
           (surface-lang (get-surface-language merged-lang start-symbol))
           (start-symbol-file (vector-ref (current-command-line-arguments) 2))
           (start-symbol-fout (open-output-file start-symbol-file #:exists 'replace)))
      (begin
        (write start-symbol start-symbol-fout)

        (write surface-lang fout)

        (close-input-port fin)
        (close-input-port reductino-relation-fin)
        (close-output-port fout)
        (close-output-port start-symbol-fout)))))

(main)