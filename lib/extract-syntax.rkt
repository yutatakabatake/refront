#lang racket
(require "common.rkt")

;; Redexのdefine-Languageだけをracketファイルに取り出す

(define start-of-def-lang 'define-language)
(define start-of-extended-lang 'define-extended-language)
(define start-of-def-reduction-relation 'reduction-relation)


(define extract-define-language
  (lambda (input-filename output-filename)
    (let* ((fin (open-input-file input-filename))
           (fout (open-output-file output-filename #:exists 'replace))
           (lang-data (esc-header fin)))
      (letrec ((search (lambda (data)
                         (cond
                           ((eof-object? data)
                            (close-input-port fin)
                            (close-output-port fout))
                           ((equal? (car data) start-of-def-lang)
                            (begin
                              (writeln (delete-mark data) fout)
                              (close-input-port fin)
                              (close-output-port fout)))
                           (else
                            (search (read fin)))))))
        (search (read fin))))))

(define extract-define-extended-language
  (lambda (input-filename output-filename)
    (let* ((fin (open-input-file input-filename))
           (fout (open-output-file output-filename #:exists 'append))
           (lang-data (esc-header fin)))
      (letrec ((search (lambda (data)
                         (cond
                           ((eof-object? data)
                            (close-input-port fin)
                            (close-output-port fout))
                           ((equal? (car data) start-of-extended-lang)
                            (begin
                              ; delete-markに渡すリストを(拡張後言語名 拡張前言語名 定義)にする
                              (writeln (append (list (car data)) (delete-mark (cdr data))) fout)
                              (search (read fin))))
                           (else
                            (search (read fin)))))))
        (search (read fin))))))

(define delete-mark
  (lambda (data)
    (let ((def (car data)) ;'define-language
          (lang-name (cadr data)))
      (letrec ((delete-mark-help
                (lambda (in-syntax out-syntax)
                  (cond
                    ((null? in-syntax)
                     (append (list def) (list lang-name) out-syntax))
                    ((member? '::= (car in-syntax)) ; ::=を消す
                     (delete-mark-help (cdr in-syntax)
                                       (append out-syntax
                                               (list (append (list (caar in-syntax))
                                                             (cdr (member '::= (car in-syntax))))))))
                    (else
                     (delete-mark-help (cdr in-syntax)
                                       (append out-syntax (list (car in-syntax)))))))))
        (delete-mark-help (cddr data) '())))))

(define extract-reduction-relation
  (lambda (input-filename output-filename)
    (let* ([fin (open-input-file input-filename)]
           [fout (open-output-file output-filename #:exists 'append)]
           [lang-data (esc-header fin)])
      (letrec ([search
                (lambda (data)
                  (cond
                    ((eof-object? data)
                     (close-input-port fin))
                    ((member? start-of-def-reduction-relation (flatten data))
                     (begin
                       (writeln data fout)
                       (close-output-port fout)))
                    (else
                     (search (read fin)))))])
        (search (read fin))))))

(define main
  (lambda ()
    (let ((input-file (vector-ref (current-command-line-arguments) 0))
          (output-file (vector-ref (current-command-line-arguments) 1)))
      (begin
        (extract-define-language input-file output-file)
        (extract-define-extended-language input-file output-file)
        (extract-reduction-relation input-file output-file)
        ))))

(main)
