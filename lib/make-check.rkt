#lang racket
(require "common.rkt")

;; LL(1)文法かどうかチェックする
;; LL(1)文法ではない場合はフィードバックする

(define get-conflict-prod-name
  (lambda (str)
    (let* ((sp (open-input-string str))
           (_ (read-line sp))
           (data1 (read-line sp))
           (data2 (read-line sp))
           (x (last (string-split data1)))
           (y (last (string-split data2)))
           (prod-name (string->symbol (substring x 0 (- (string-length x) 2))))
           (other (string->symbol (substring y 0 (- (string-length y) 2)))))
      (list prod-name other))))

(define get-production
  (lambda (grammar prod-names)
    (filter (lambda (elem)
              (member? (last elem) prod-names))
            grammar)))


(define rewrite
  (lambda (grammar new-prod new-grammar)
    (cond
      ((null? grammar)
       (append '(define grammar) (list (list 'quote new-grammar))))
      ((equal? (last (car grammar)) (last new-prod))
       (append '(define grammar) (list (list 'quote (append* new-grammar (list new-prod) (list (cdr grammar)))))))
      (else
       (rewrite (cdr grammar) new-prod (append new-grammar (list (car grammar))))))))

(define main
  (lambda ()
    (begin
      (let* ([input-file (vector-ref (current-command-line-arguments) 0)])

        (with-handlers
            ([exn:fail?
              (lambda (exn)
                (let* ([input-file (vector-ref (current-command-line-arguments) 0)]
                       [output-file (vector-ref (current-command-line-arguments) 0)]
                       [fin (open-input-file input-file)]
                       [grammar (car (cdr (car (cddr (extract-grammar fin)))))]
                       [conf-rules (get-conflict-prod-name (exn-message exn))]
                       [conf-prods (get-production grammar conf-rules)])
                  (begin
                    (close-input-port fin)
                    (displayln "Not LL(1) grammar!!!!!!")
                    (if (null? conf-prods)
                        (begin ; 想定外のエラーメッセージはそのまま表示して終了
                          (newline)
                          (displayln (exn-message exn))
                          (displayln "It is not apparent which rule to modify. Please review the grammar.")
                          (exit 1))
                        (begin
                          (display "conflict productions :")
                          (displayln conf-prods)
                          (display "Enter fixed production (or type \"stop\" to abort) :")
                          (let ([direction (read)])
                            (displayln direction)
                            ; stopと入力されたらその時点でのgrammarを保存して終了
                            (if (equal? direction 'stop)
                                (begin
                                  (displayln "stop fixing grammar")
                                  (displayln "Please review the grammar.")
                                  (exit 1))
                                (begin
                                  (let ([new-grammar (rewrite grammar direction '())]
                                        [fout (open-output-file output-file #:exists 'replace)])
                                    (displayln new-grammar)
                                    (newline)
                                    (displayln "#lang eopl" fout)
                                    (displayln '(provide (all-defined-out)) fout)
                                    (displayln '(define scan&parse (sllgen:make-string-parser scanner-spec grammar)) fout)
                                    (writeln '(define scanner-spec (quote ((whitespace (whitespace) skip) (comment ("%" (arbno (not #\newline))) skip) (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol) (number (digit (arbno digit)) number) (number ("-" digit (arbno digit)) number) (string ("\"" letter (arbno (or letter digit whitespace "_" "-" "?")) "\"") string)))) fout)
                                    (writeln new-grammar fout)
                                    (display '(sllgen:make-define-datatypes scanner-spec grammar) fout)
                                    (close-output-port fout)
                                    (main))))))))))])
          (load input-file)
          (display "LL(1) grammar!!!!!"))))))

(main)