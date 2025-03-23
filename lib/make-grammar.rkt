#lang racket
(require "common.rkt")

;; SLLGEN用表層言語の文法を作る



; Redexで使えるパターン
(define patterns '(number natural integer real string boolean))

(define parser
  '(define scan&parse
     (sllgen:make-string-parser scanner-spec grammar)))

(define scanner-spec
  '(define scanner-spec
     '((whitespace (whitespace) skip)
       (comment ("%" (arbno (not #\newline))) skip)
       (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
       (identifier ("_") symbol)
       (number (digit (arbno digit)) number)
       (number ("-" digit (arbno digit)) number)
       (string ("\"" letter (arbno (or letter digit whitespace "_" "-" "?")) "\"") string))))


; variants = (cddr data)
; define-languageと言語名を除いたリスト
(define make-grammar
  (lambda (variants output nonterminal-symbols variable-symbols)
    (cond
      ((null? variants)
       (string->sexpr (string-append output "))")))
      (else
       (make-grammar (cdr variants)
                     (string-append output (make-production (car variants) nonterminal-symbols variable-symbols))
                     nonterminal-symbols
                     variable-symbols)))))

; datatype = (car (cddr data)) ex. (v number boolean)
; rule = (cdr (car (cddr data))) ex. (number boolean)
(define make-production
  (lambda (datatype nonterminal-symbols variable-symbols)
    (let ((lhs (car datatype)))
      (if (member? lhs variable-symbols)
          "" ; 変数の場合はproductionは不要
          (letrec ((loop
                    (lambda (rule output)
                      (cond
                        ((null? rule)
                         output)
                        ((list? (car rule))
                         (loop (cdr rule)
                               (string-append* output
                                               "("
                                               (symbol->string lhs)
                                               "("
                                               (make-rhs-item-list (car rule) nonterminal-symbols variable-symbols)
                                               ")"
                                               (string-append* (symbol->string (car (flatten (car rule))))
                                                               (number->string (random 100))
                                                               "-" (symbol->string lhs) '())
                                               ")" '())))
                        ((symbol? (car rule))
                         (if (eq? (car rule) 'boolean) ; booleanは非終端記号bを作る
                             (loop (cdr rule)
                                   (string-append output
                                                  "(" (symbol->string lhs)
                                                  "(" "b" ")"
                                                  (string-append "b-" (symbol->string lhs)) ")"
                                                  "( b (true) true-b)"
                                                  "( b (false) false-b)"
                                                  ))
                             (loop (cdr rule)
                                   (string-append* output
                                                   "("
                                                   (symbol->string lhs)
                                                   "("
                                                   (make-rhs-item-symbol (car rule) nonterminal-symbols variable-symbols)
                                                   ")"
                                                   (string-append* (symbol->string (car rule))
                                                                   (number->string (random 100))
                                                                   "-" (symbol->string lhs) '())
                                                   ")" '()))))))))
            (loop (cdr datatype) ""))))))

;let,複数引数関数　separated-listに未対応
(define make-rhs-item-list
  (lambda (item-lst nonterminal-symbols variable-symbols)
    (letrec ((loop
              (lambda (item-lst rhs-output)
                (cond
                  ((null? item-lst)
                   rhs-output)
                  ((list? (car item-lst))
                   (loop (cdr item-lst)
                         (string-append* rhs-output " " "(" (make-rhs-item-list (car item-lst) nonterminal-symbols variable-symbols) ")" '())))
                  ((symbol? (car item-lst))
                   (loop (cdr item-lst)
                         (string-append* rhs-output " " (make-rhs-item-symbol (car item-lst) nonterminal-symbols variable-symbols) '())))))))
      (loop item-lst ""))))

(define make-rhs-item-symbol
  (lambda (sym nonterminal-symbols variable-symbols)
    (cond
      ((member? sym variable-symbols)
       "identifier")
      ((and (member? (string->symbol (car (string-split (symbol->string sym) "_")))
                     nonterminal-symbols) ;_を使う非終端記号なら_以下を消す
            (string-contains? (symbol->string sym) "_"))
       (make-rhs-item-symbol (string->symbol (string (string-ref (symbol->string sym) 0)))
                             nonterminal-symbols
                             variable-symbols))
      (else
       (symbol->string sym)))))

(define fix-grammar
  (lambda (wrong-grammar nonterminal-symbols)
    (list 'define 'grammar (list 'quote (fix-productions (cadar (cddr wrong-grammar)) '() nonterminal-symbols)))))

(define fix-productions
  (lambda (in-productions out-productions nonterminal-symbols)
    (cond
      ((null? in-productions)
       out-productions)
      (else
       (fix-productions (cdr in-productions)
                        (append out-productions (list (fix-production (car in-productions) nonterminal-symbols)))
                        nonterminal-symbols)))))

(define fix-production
  (lambda (prod nonterminal-symbols)
    (let ((lhs (car prod))
          (rhs (cadr prod))
          (prod-name (caddr prod)))
      (list lhs (fix-rhs rhs '() nonterminal-symbols) prod-name))))

(define fix-rhs
  (lambda (in-rhs out-rhs nonterminal-symbols)
    (cond
      ((null? in-rhs)
       (parentheses->str out-rhs '()))
      ((list? (car in-rhs))
       (fix-rhs (cdr in-rhs)
                (append out-rhs (list (fix-rhs (car in-rhs) '() nonterminal-symbols)))
                nonterminal-symbols))
      ((symbol? (car in-rhs))
       (let ((sym (car in-rhs)))
         (cond
           ((or (member? sym nonterminal-symbols)
                (member? sym patterns)
                (equal? sym 'identifier))
            (cond
              ((or (equal? sym 'natural)
                   (equal? sym 'integer)
                   (equal? sym 'real))
               (fix-rhs (cdr in-rhs) (append out-rhs (list 'number)) nonterminal-symbols)) ;sllgenで使えるように全てnumberにする
              ((equal? sym 'boolean)
               (fix-rhs (cdr in-rhs) (append out-rhs (list 'b)) nonterminal-symbols))
              (else
               (fix-rhs (cdr in-rhs) (append out-rhs (list sym)) nonterminal-symbols))))
           ((equal? sym '...)
            (let ((tmp-out-rhs (reverse (cdr (reverse out-rhs))))) ; ...の直前の要素を消す
              (if (list? (last out-rhs))
                  (fix-rhs (cdr in-rhs)
                           (append tmp-out-rhs (list (append (list 'arbno) (last out-rhs))))
                           nonterminal-symbols)
                  (fix-rhs (cdr in-rhs)
                           (append tmp-out-rhs (list (list 'arbno (last out-rhs))))
                           nonterminal-symbols))))
           ((equal? sym 'l-brace)
            (fix-rhs (cdr in-rhs) (append out-rhs (list "{")) nonterminal-symbols))
           ((equal? sym 'r-brace)
            (fix-rhs (cdr in-rhs) (append out-rhs (list "}")) nonterminal-symbols))
           ((equal? sym 'l-pare)
            (fix-rhs (cdr in-rhs) (append out-rhs (list "(")) nonterminal-symbols))
           ((equal? sym 'r-pare)
            (fix-rhs (cdr in-rhs) (append out-rhs (list ")")) nonterminal-symbols))
           ((equal? sym 'l-bracket)
            (fix-rhs (cdr in-rhs) (append out-rhs (list "[")) nonterminal-symbols))
           ((equal? sym 'r-bracket)
            (fix-rhs (cdr in-rhs) (append out-rhs (list "]")) nonterminal-symbols))
           ((equal? sym 'semicolon)
            (fix-rhs (cdr in-rhs) (append out-rhs (list ";")) nonterminal-symbols))
           ((equal? sym 'dot)
            (fix-rhs (cdr in-rhs) (append out-rhs (list ".")) nonterminal-symbols))
           (else
            (fix-rhs (cdr in-rhs) (append out-rhs (list (symbol->string sym))) nonterminal-symbols))))))))

;; リストの要素がリストなら()を文字列の"(" ")"にする
;; ex. (parentheses->str '(1 2 (3 4) 5) '()) -> '(1 2 "(" 3 4 ")" 5)
(define parentheses->str
  (lambda (lst out-lst)
    (cond
      ((null? lst)
       out-lst)
      ((list? (car lst))
       (cond
         ((or (member? 'arbno (car lst)) ; sllgenのpattern keywordsを使う場合はリストのままにする
              (member? 'separated-list (car lst)))
          (parentheses->str (cdr lst) (append out-lst (list (car lst)))))
         ((or (member? "{" (car lst)) ; {}を使うときは元のリストの()は消す
              (member? "}" (car lst)))
          (parentheses->str (cdr lst) (append out-lst (car lst))))
         (else
          (parentheses->str (cdr lst) (append out-lst (append (list "(") (car lst) (list ")")))))))
      (else
       (parentheses->str (cdr lst) (append out-lst (list (car lst))))))))


(define main
  (lambda ()
    (let* ((input-file (vector-ref (current-command-line-arguments) 0))
           (output-file (vector-ref (current-command-line-arguments) 1))
           (fin (open-input-file input-file))
           (fout (open-output-file output-file #:exists 'replace))
           (data (read fin))
           (boolean-flag (member? 'boolean (flatten data)))
           (_ (close-input-port fin))
           (variable-symbols (variable-symbols-of data))
           (nonterminal-symbols (if boolean-flag
                                    (append (nonterminal-symbols-of data) '(b))
                                    (nonterminal-symbols-of data)))
           (start-symbol-file (vector-ref (current-command-line-arguments) 2))
           (start-symbol-fin (open-input-file start-symbol-file))
           (start-symbol (read start-symbol-fin))
           (_ (close-input-port start-symbol-fin))
           (fix-output-file (vector-ref (current-command-line-arguments) 3))
           (fix-fout (open-output-file fix-output-file #:exists 'replace)))
      (begin
        (displayln "#lang eopl" fout)
        (displayln "#lang eopl" fix-fout)

        (writeln '(provide (all-defined-out)) fout)
        (writeln parser fout)
        (newline fout)

        (writeln '(provide (all-defined-out)) fix-fout)
        (writeln parser fix-fout)
        (newline fix-fout)

        (writeln scanner-spec fout)
        (newline fout)

        (writeln scanner-spec fix-fout)
        (newline fix-fout)

        (define first-transformed-list
          (make-grammar (cddr data)
                        (string-append* "(define grammar '((program ("
                                        (symbol->string start-symbol)
                                        ") a-program)"
                                        '())
                        nonterminal-symbols variable-symbols))
        (writeln (fix-grammar first-transformed-list nonterminal-symbols) fout)
        (newline fout)

        (writeln (fix-grammar first-transformed-list nonterminal-symbols) fix-fout)
        (newline fix-fout)

        (write '(sllgen:make-define-datatypes scanner-spec grammar) fout)
        (close-output-port fout)

        (write '(sllgen:make-define-datatypes scanner-spec grammar) fix-fout)
        (close-output-port fix-fout)))))

(main)