#lang racket
(require "common.rkt")
; OCmalでのデータ型を表すsyntax.mlを作る
; ファイル末尾は改行2個

(define make-id
  (lambda (fout)
    (displayln "type id = string" fout)))

(define make-syntax
  (lambda (productions variant-name-list variable-symbols nonterminal-symbols fout)
    (cond
      ((null? productions)
       '())
      ((member (caar productions) variable-symbols)
       (make-syntax (cdr productions)
                    (cdr variant-name-list)
                    variable-symbols
                    nonterminal-symbols
                    fout))
      (else
       (begin
         (displayln (string-append "and " (symbol->string (caar productions))
                                   " = \n"
                                   (make-syntax-help (cdar productions)
                                                     (car variant-name-list)
                                                     variable-symbols
                                                     nonterminal-symbols
                                                     ""
                                                     fout)
                                   )
                    fout)
         (make-syntax (cdr productions)
                      (cdr variant-name-list)
                      variable-symbols
                      nonterminal-symbols
                      fout))))))

(define make-syntax-help
  (lambda (rhs variant-names variable-symbols nonterminal-symbols output fout)
    (cond
      ((null? rhs)
       output)
      ((null? variant-names)
       output)
      ((symbol? (car rhs))
       (let ((sym (remove-underscore (car rhs))))
         (cond
           ((equal? sym 'boolean)
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) " of b\n")
                              fout))
           ((member? sym '(integer number natural real))
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) " of int\n")
                              fout))
           ((equal? sym 'string)
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) " of string\n")
                              fout))
           ((member? sym variable-symbols)
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) " of id\n")
                              fout))
           ((member? sym nonterminal-symbols)
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) " of " (symbol->string sym) "\n")
                              fout))
           (else
            (make-syntax-help (cdr rhs)
                              (cdr variant-names)
                              variable-symbols
                              nonterminal-symbols
                              (string-append output
                                             "| " (car variant-names) "\n")
                              fout)))))
      ((list? (car rhs))
       (let* ((lst (car rhs)))
         (let ((fields (flatten (make-field lst variable-symbols nonterminal-symbols '() fout))))
           (if (null? fields)
               (make-syntax-help (cdr rhs)
                                 (cdr variant-names)
                                 variable-symbols
                                 nonterminal-symbols
                                 (string-append output
                                                "| " (car variant-names) "\n")
                                 fout)
               (make-syntax-help (cdr rhs)
                                 (cdr variant-names)
                                 variable-symbols
                                 nonterminal-symbols
                                 (string-append output
                                                "| " (car variant-names) " of "
                                                (string-append* (cdr (append* (map (lambda (x) (list " * " x))
                                                                                   (map (lambda (sym) (symbol->string sym))
                                                                                        fields)))))
                                                "\n")
                                 fout))))))))

(define make-field
  (lambda (fields variable-symbols nonterminal-symbols output fout)
    (cond
      ((null? fields)
       output)
      ((list? (car fields))
       (make-field (cdr fields)
                   variable-symbols
                   nonterminal-symbols
                   (append output (list (make-field (car fields)
                                                    variable-symbols
                                                    nonterminal-symbols
                                                    '()
                                                    fout)))
                   fout))
      ((symbol? (car fields))
       (let ((sym (remove-underscore (car fields))))
         (cond
           ((member? sym variable-symbols)
            (make-field (cdr fields)
                        variable-symbols
                        nonterminal-symbols
                        (append output (list 'id))
                        fout))
           ((equal? sym 'boolean)
            (make-field (cdr fields)
                        variable-symbols
                        nonterminal-symbols
                        (append output (list 'b))
                        fout))
           ((member? sym nonterminal-symbols)
            (make-field (cdr fields)
                        variable-symbols
                        nonterminal-symbols
                        (append output (list sym))
                        fout))
           ((equal? sym '...)
            (let ((arb-item (last output)))
              (cond
                ((symbol? arb-item)
                 (let ((generated-sym (gensym)))
                   (displayln (write-gen generated-sym (last output)) fout)
                   (newline fout)
                   (make-field (cdr fields)
                               variable-symbols
                               nonterminal-symbols
                               (append (reverse (cdr (reverse output)))
                                       (list (string->symbol (string-append
                                                              (symbol->string generated-sym)
                                                              " list"))))
                               fout)))
                ((list? arb-item)
                 (let ((generated-sym (gensym)))
                   (displayln (write-gen generated-sym (last output)) fout)
                   (newline fout)

                   (make-field (cdr fields)
                               variable-symbols
                               nonterminal-symbols
                               (append (reverse (cdr (reverse output)))
                                       (list (string->symbol (string-append
                                                              (symbol->string generated-sym)
                                                              " list"))))
                               fout)
                   )))))
           (else
            (make-field (cdr fields)
                        variable-symbols
                        nonterminal-symbols
                        output
                        fout))))))))

(define write-gen
  (lambda (sym old-fields)
    (let* ((str (symbol->string sym))
           (variant (up-first-char str)))
      (string-append* "and "
                      str
                      " =\n|\t"
                      variant
                      " of "
                      (cdr (append* (map (lambda (x) (list " * " x))
                                         (map (lambda (sym) (symbol->string sym))
                                              (flatten old-fields)))))))))

(define extract-variant-name
  (lambda (input-line-list)
    (cond
      [(null? input-line-list)
       '()]
      [(equal? (car input-line-list) '|{|)
       (second input-line-list)]
      [else
       (extract-variant-name (cdr input-line-list))])))

(define get-variant-names
  (lambda (yacc-data)
    (map (lambda (x)
           (cond
             [(null? x)
              '()]
             [else
              (symbol->string x)]))
         (map extract-variant-name yacc-data))))

(define remove-empty-lists
  (lambda (lst)
    (cond
      [(empty? lst) empty] ; リストが空なら空リストを返す
      [(empty? (first lst)) (remove-empty-lists (rest lst))] ; 先頭が空リストなら再帰的に処理
      [else (cons (first lst) (remove-empty-lists (rest lst)))]))) ; それ以外の場合、先頭の要素を残して再帰的に処理

(define replace
  (lambda (variant-name-list yacc-variants result)
    (cond
      [(null? variant-name-list)
       result]
      [else
       (replace (cdr variant-name-list)
                (drop yacc-variants (length (car variant-name-list)))
                (append result (list (take yacc-variants (length (car variant-name-list))))))])))

(define main
  (lambda ()
    (let* ((input-file (vector-ref (current-command-line-arguments) 0))
           (output-file (vector-ref (current-command-line-arguments) 1))
           (fin (open-input-file input-file))
           (fout (open-output-file output-file #:exists 'replace))
           (data (read fin))
           (productions (cddr data))
           (variable-symbols (variable-symbols-of data))
           (boolean-flag (member? 'boolean (flatten data)))
           (nonterminal-symbols (if (null? boolean-flag)
                                    (nonterminal-symbols-of data)
                                    (append (nonterminal-symbols-of data) '(b))))
           (start-symbol-file (vector-ref (current-command-line-arguments) 2))
           (start-symbol-fin (open-input-file start-symbol-file))
           (start-symbol (read start-symbol-fin))
           (yacc-file (vector-ref (current-command-line-arguments) 3))
           (yacc-fin (open-input-file yacc-file))
           (yacc-data (ocamlyacc-reader yacc-fin))
           (variant-name-list-yacc (remove-empty-lists (get-variant-names (cddr yacc-data))))
           (_ (close-input-port start-symbol-fin))
           (_ (close-input-port yacc-fin))
           (variant-name-list (make-variant-name productions '()))
           (new-variant-name-list (replace variant-name-list variant-name-list-yacc '())))
      (begin
        (make-id fout)
        (newline fout)

        (displayln (string-append "type program = A_program of " (symbol->string start-symbol)) fout)

        (make-syntax productions new-variant-name-list variable-symbols nonterminal-symbols fout)

        (if boolean-flag
            (displayln "and b =\n| Boolean_b of bool\n"
                       fout)
            '())

        (close-input-port fin)
        (close-output-port fout)))))

(main)
