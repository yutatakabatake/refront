#lang racket
;(require "./../common.rkt")
(require "common.rkt")

;; ASTからS式に変換する関数を作る

;; 非終端記号ごとに変換関数を書き出す
(define make-converter
  (lambda (grouped-grammar fout nonterminal-symbols redex-productions)
    (cond
      ((null? grouped-grammar)
       (close-output-port fout))
      (else
       (begin
         (writeln (insert-append (make-converter-for-nonterminal-symbol (car grouped-grammar) nonterminal-symbols (car redex-productions)) '() '()) fout)
         (newline fout)
         (make-converter (cdr grouped-grammar) fout nonterminal-symbols (cdr redex-productions)))))))

;; 一つの終端記号に対して変換関数を作る
;; productions = (car group-by-nonterminal-symbols)
;; ex. ((program (c) a-program)), ((v (identifier) x-v) (v ("true") true-v) ...)
(define make-converter-for-nonterminal-symbol
  (lambda (productions nonterminal-symbols redex-production)
    (if (boolean? redex-production)
        (list 'define 'b-to-list
              (list 'lambda
                    (list 'b1)
                    (list 'cases 'b 'b1
                          (list 'true-b (list) #t)
                          (list 'false-b (list) #f))))
        (let ((lhs (caar productions)))
          (list 'define
                (string->symbol (string-append* (symbol->string lhs) "-to-list" '()))
                (list 'lambda
                      (list (string->symbol (string-append (symbol->string lhs) "1")))
                      (append (list 'cases lhs (string->symbol (string-append (symbol->string lhs) "1")))
                              (append-converter-for-productions productions '() nonterminal-symbols (cdr redex-production)))))))))

;; 複数のproductionから作られる複数のvariantを一つにまとめる
(define append-converter-for-productions
  (lambda (productions converter nonterminal-symbols redex-rhss)
    (cond
      ((null? productions)
       converter)
      (else
       (append-converter-for-productions (cdr productions)
                                         (append converter (list (make-converter-for-produciton (car productions) nonterminal-symbols (car redex-rhss))))
                                         nonterminal-symbols
                                         (cdr redex-rhss))))))

;; 1つのproductionから1つのvariantを作る
;; production = (car productions)
;; ex. (v (identifier) x-v), (v ("fun" identifier "->" c) fun-v)
;; separated-list 未対応
(define make-converter-for-produciton
  (lambda (production nonterminal-symbols redex-rhs)
    (let ((rhs (cadr production))
          (prod-name (caddr production)))
      (cond
        ; rhsの要素が1つである
        ((equal? (length rhs) 1)
         (if (symbol? redex-rhs)
             ; redexで両端にカッコをつけていない場合
             (cond
               ;rhsの要素が文字列である
               ((string? (car rhs))
                (let ((field-id '())
                      (result-expr (string->symbol (car rhs))))
                  (list prod-name field-id (list 'quote result-expr))))
               ;rhsの要素が非終端記号である
               ((member? (car rhs) nonterminal-symbols)
                (let ((rhs-item (car rhs)))
                  (list prod-name rhs (list (string->symbol (string-append* (symbol->string rhs-item) "-to-list" '())) rhs-item)))) ;非終端記号はその記号の変換関数に渡す
               ;rhsの要素がidentifier,numberのいずれかである
               ((or (equal? (car rhs) 'identifier)
                    (equal? (car rhs) 'number))
                (list prod-name rhs (car rhs)))
               ;stringの場合は先頭と末尾を1字ずつ消す
               ((equal? (car rhs) 'string)
                (list prod-name rhs '(substring string 1 (- (string-length string) 1)))))
             ; redexで両端にカッコをつけている場合
             (cond
               ;rhsの要素が文字列である
               ((string? (car rhs))
                (let ((field-id '())
                      (result-expr (string->symbol (car rhs))))
                  (list prod-name field-id (append '(list) (list (list 'quote result-expr))))))
               ;rhsの要素が非終端記号である
               ((member? (car rhs) nonterminal-symbols)
                (let ((rhs-item (car rhs)))
                  (list prod-name rhs (append '(list) (list (list (string->symbol (string-append* (symbol->string rhs-item) "-to-list" '())) rhs-item)))))) ;非終端記号はその記号の変換関数に渡す
               ;rhsの要素がidentifier,numberのいずれかである
               ((or (equal? (car rhs) 'identifier)
                    (equal? (car rhs) 'number))
                (list prod-name rhs (append '(list) (list (car rhs)))))
               ;stringの場合は先頭と末尾を1字ずつ消す
               ((equal? (car rhs) 'string)
                (list prod-name rhs (append '(list) (list '(substring string 1 (- (string-length string) 1))))))
               ;rhsがarbnoを使う場合
               ((equal? (caar rhs) 'arbno)
                (let* ([flat (flatten rhs)]
                       [field (make-field (remove 'arbno flat))])
                  (list prod-name field
                        (make-result rhs field '(list) nonterminal-symbols)))))))
        ;rhsの要素が複数でリストではない(arbno,separated-listではない)場合
        ((andmap (lambda (elem) (not (list? elem))) rhs)
         (let ((field (make-field rhs)))
           (list prod-name field (make-result rhs field '(list) nonterminal-symbols))))
        ;rhsの要素がリスト(今はarbnoだけ)を含む場合
        ((ormap (lambda (elem) (list? elem)) rhs)
         (let* ((flat (flatten rhs))
                (field (make-field (remove 'arbno flat))))
           (list prod-name field (make-result rhs field '(list) nonterminal-symbols))))))))

(define make-field
  (lambda (rhs)
    (let ((symbols (filter symbol? rhs)))
      (enumerate-symbols-recursive symbols))))

;; mapの引数のprocのbodyを作る
(define make-arbno-to-list-core
  (lambda (arb-lst nonterminal-symbols)
    (letrec ((loop
              (lambda (arb-lst out)
                (cond
                  ((null? arb-lst)
                   (if (equal? (length out) 1)
                       (car out)
                       (cons 'list (convert-parentheses out))))
                  ((equal? (car arb-lst) 'arbno)
                   (loop (cdr arb-lst) out))
                  ((string? (car arb-lst))
                   (loop (cdr arb-lst) (append out (list (elem->symbol (car arb-lst))))))
                  ;; arbnoの要素がidentifierの場合はそのままにする
                  ((string-contains? (symbol->string (car arb-lst)) "identifier")
                   (loop (cdr arb-lst) (append out (list (car arb-lst)))))
                  ;;非終端記号は-to-listに渡す
                  ((ormap (lambda (elem)
                            (string-contains? (symbol->string (car arb-lst)) elem))
                          (map symbol->string nonterminal-symbols))
                   (loop (cdr arb-lst) (append out (list
                                                    (list (string->symbol (string-append (symbol->string (strip-symbol (car arb-lst)))
                                                                                         "-to-list"))
                                                          (car arb-lst))))))
                  ((symbol? (car arb-lst))
                   (loop (cdr arb-lst) (append out (list (car arb-lst)))))))))
      (loop arb-lst '()))))

(define make-result
  (lambda (rhs field result nonterminal-symbols)
    (cond
      ;;全てのfield-idを変換した
      ((null? field)
       (convert-parentheses (append result (map elem->symbol rhs))))
      ((string? (car rhs))
       (make-result (cdr rhs) field (append result (list (elem->symbol (car rhs)))) nonterminal-symbols))
      ;要素がリスト(arbno)の場合
      ((list? (car rhs))
       (let* ((tmp (make-new-rhs-and-field rhs field nonterminal-symbols))
              (new-field (car tmp))
              (new-rhs (second tmp)))
         (make-result (cdr new-rhs)
                      new-field
                      (append result (list
                                      (append (list 'map
                                                    (list 'lambda (remove* new-field field) (make-arbno-to-list-core (car new-rhs) nonterminal-symbols)))
                                              (remove* new-field field))))
                      nonterminal-symbols)))
      ((or (equal? (car rhs) 'identifier)
           (equal? (car rhs) 'number)
           (equal? (car rhs) 'string))
       (make-result (cdr rhs) (cdr field) (append result (list (car field))) nonterminal-symbols))
      (else
       (make-result (cdr rhs) (cdr field) (append result (list (list (string->symbol (string-append (symbol->string (car rhs)) "-to-list")) (car field)))) nonterminal-symbols)))))

;; ; (arbno ...)の...の部分をfieldから削除する
(define make-new-rhs-and-field
  (lambda (rhs field nonterminal-symbols)
    ; arbno-list = (arbno ...)
    (let* ([arbno-list (car rhs)]
           [arbno-elems (cdr arbno-list)])
      (let* ([tmp (make-new-rhs-and-field-helper arbno-elems field nonterminal-symbols '())]
             [new-field (car tmp)]
             [new-arbno-elems (second tmp)]
             [new-arbno-list (cons 'arbno new-arbno-elems)]
             [new-rhs (cons new-arbno-list (cdr rhs))])
        (list new-field new-rhs)))))

(define make-new-rhs-and-field-helper
  (lambda (arbno-elems field nonterminal-symbols result)
    (cond
      ((null? arbno-elems)
       (list field result))
      ((member? (car arbno-elems) (append nonterminal-symbols (list 'identifier)))
       (make-new-rhs-and-field-helper (cdr arbno-elems)
                                      (cdr field)
                                      nonterminal-symbols
                                      (append result (list (car field)))))
      (else
       (make-new-rhs-and-field-helper (cdr arbno-elems)
                                      field
                                      nonterminal-symbols
                                      (append result (list (car arbno-elems))))))))

; 表層言語用識別子への変換
(define elem->symbol
  (lambda (elem)
    (cond
      ((equal? elem ";")
       (list 'quote 'semicolon))
      ((equal? elem ".")
       (list 'quote 'dot))
      ((equal? elem "{")
       (list 'quote 'l-brace))
      ((equal? elem "}")
       (list 'quote 'r-brace))
      ((equal? elem "[")
       (list 'quote 'l-bracket))
      ((equal? elem "]")
       (list 'quote 'r-bracket))
      ((or (equal? elem "(")
           (equal? elem ")"))
       elem)
      (else
       (list 'quote (string->symbol elem))))))

(define convert-parentheses
  (lambda (lst)
    (convert-parentheses-help lst '())))

(define convert-parentheses-help
  (lambda (lst acc)
    (cond
      [(null? lst)
       (reverse acc)]
      ; カッコの始まりをみつけたら残りのリストを変換する
      [(equal? (car lst) "(")
       (let ([result (extract-sublist (cdr lst) '())])
         (convert-parentheses-help (cdr result) (cons (car result) acc)))]
      [else
       (convert-parentheses-help (cdr lst) (cons (car lst) acc))])))

; カッコで囲まれた部分をリストに変換する
(define extract-sublist
  (lambda (lst acc)
    (cond
      [(null? lst)
       (cons (reverse acc) lst)]
      [(equal? (car lst) ")")
       (cons (cons 'list (reverse acc)) (cdr lst))]
      [(equal? (car lst) "(")
       (let ([result (extract-sublist (cdr lst) '())])
         (extract-sublist (cdr result) (cons (car result) acc)))]
      [else
       (extract-sublist (cdr lst) (cons (car lst) acc))])))

(define insert-append
  (lambda (lst tmp out)
    (cond
      ((null? lst)
       out)
      (else
       (let ((elem (car lst)))
         (cond
           ((or (symbol? elem)
                (null? elem)
                (not (list? elem)))
            (insert-append (cdr lst) elem (append out (list elem))))
           ;;mapを見つけて直前の要素がsymbolの場合は空リストにappendする
           ((and (equal? (car elem) 'map)
                 (symbol? tmp))
            (if (equal? (car out) 'list)
                (insert-append (cdr lst)
                               elem
                               (append (list-tail out 1) (list 'append (list 'quote '()) elem)))
                (insert-append (map (lambda (elem)
                                      (list 'list elem))
                                    (cdr lst))
                               elem
                               (append out (list elem)))))
           ;;mapを見つけて直前の要素がlistの場合
           ((and (equal? (car elem) 'map)
                 (list? tmp))
            (insert-append (map (lambda (elem)
                                  (list 'list elem))
                                (cdr lst))
                           elem
                           (append (list 'append (drop-right out 1)) (list (list 'append (list 'list tmp) elem))))) ;outの先頭にappendをつける　mapの一つ前の要素aを(append list a)にする mapと同じ階層のmap以降のリストの要素を全てlistにする
           ;; 先頭が ''symbol の場合
           ((and (list? elem)
                 (equal? 'quote (car elem)))
            (insert-append (cdr lst) elem (append out (list elem))))

           ((list? elem)
            (insert-append (cdr lst) elem (append out (list (insert-append elem '() '())))))))))))

; 数字と文字列が混ざったシンボルを文字列だけのシンボルにする
(define strip-symbol
  (lambda (sym)
    (let* ([str (symbol->string sym)]              ; シンボルを文字列に変換
           [str-without-digits (regexp-replace* #rx"[0-9]+$" str "")]) ; 数字を削除
      (string->symbol str-without-digits))))

(define remove-meta-variables
  (lambda (productions result)
    (cond
      [(null? productions)
       result]
      [(or (equal? (second (car productions)) 'variable-not-otherwise-mentioned)
           (equal? (second (car productions)) 'variable))
       (remove-meta-variables (cdr productions) result)]
      [else
       (remove-meta-variables (cdr productions) (append result (list (car productions))))])))

(define rearrange
  (lambda (list1 list2)
    (let ([key (map car (map car list1))])
      (map (lambda (key) (assoc key list2)) key))))


(define main
  (lambda ()
    (let* ((input-file (vector-ref (current-command-line-arguments) 0))
           (output-file (vector-ref (current-command-line-arguments) 1))
           (fix-grammar-file (vector-ref (current-command-line-arguments) 2))
           (fin (open-input-file input-file))
           (fout (open-output-file output-file #:exists 'replace))
           (grammar (car (cdr (car (cddr (extract-grammar fin))))))
           (nonterminal-symbols
            (remove-duplicates (nonterminal-symbols-of (append (list 'dummy) (list 'dummy) grammar))))
           ;; 非終端記号ごとにgrammarを切り分ける
           (group-by-nonterminal-symbols (group-by car grammar))
           (merged-lang-file (vector-ref (current-command-line-arguments) 3))
           (merged-lang-fin (open-input-file merged-lang-file))
           (merged-lang (read merged-lang-fin))
           (_ (close-input-port merged-lang-fin))
           (merged-productions (cons '(program dummy) (remove-meta-variables (cddr merged-lang) '())))
           (rearranged-merged-productions (rearrange group-by-nonterminal-symbols merged-productions)))
      (begin
        (display "#lang eopl\n" fout)
        (writeln `(require ,fix-grammar-file) fout)
        (writeln `(provide program-to-list) fout)
        (writeln `(provide s&p) fout)
        (make-converter group-by-nonterminal-symbols fout nonterminal-symbols rearranged-merged-productions)
        (define a (open-output-file output-file #:exists 'append))
        ; (writeln '(define main
        ;             (let* ((fout (open-output-file output-file #:exists 'replace)))
        ;               (write (program-to-list AST) fout)
        ;               (close-output-port fout))) a)
        (writeln '(define s&p scan&parse) a)
        (close-output-port a)))))

(main)
