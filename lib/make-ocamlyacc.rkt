#lang racket
(require "common.rkt")

(define gensym-list '())
(define rec-list '())
(define boolean-variant '())

(define make-open-syntax
  (lambda (fout)
    (displayln "%{\n\topen Syntax\n%}" fout)))

(define make-num-id-string-token
  (lambda (fout)
    (displayln "%token<int> NUM\n%token<Syntax.id> ID\n%token<string> STR" fout)))

; 第1引数は要素がシンボルまたはリスト，返すリストは要素が文字列だけのリスト
; 第1引数で受け取ったリストからトークンを要素として持つリストを作る
(define get-tokens
  (lambda (input-list variable-symbols nonterminal-symbols)
    ; (display "input-list :")
    ; (writeln input-list)
    (get-tokens-help
     input-list
     variable-symbols
     nonterminal-symbols
     '())))

(define get-tokens-help
  (lambda (input-list variable-symbols nonterminal-symbols tokens)
    (cond
      ((null? input-list)
       (flatten tokens))
      ((list? (car input-list))
       (get-tokens-help (cdr input-list)
                        variable-symbols
                        nonterminal-symbols
                        (append tokens (list (get-tokens-help (car input-list)
                                                              variable-symbols
                                                              nonterminal-symbols
                                                              '())))))
      ((symbol? (car input-list))
       (let ((str (symbol->string (car input-list))))
         (cond
           ((equal? str "+")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "PLUS"))))
           ((equal? str "-")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "MINUS"))))
           ((equal? str "*")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "MULT"))))
           ((equal? str "/")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "DIV"))))
           ((equal? str "=")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "EQ"))))
           ((equal? str "<")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "LT"))))
           ((equal? str "<=")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "LE"))))
           ((equal? str ">")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "GT"))))
           ((equal? str ">=")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "GE"))))
           ((equal? str "!=")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "NEQ"))))
           ((equal? str "==")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "EQEQ"))))
           ((equal? str "^")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "CARET"))))
           ((equal? str "->")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "RARROW"))))
           ((equal? str "<-")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "LARROW"))))
           ((equal? str "string")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "STR"))))
           ((or (equal? str "number")
                (equal? str "integer")
                (equal? str "natural")
                (equal? str "real"))
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "NUM"))))
           ((equal? str "boolean")
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "b"))))
           ((member? (string->symbol str) variable-symbols)
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list "ID"))))
           ((member? (string->symbol str) nonterminal-symbols)
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list str))))
           ((string-contains? str "_")
            (let ((sym (remove-underscore (string->symbol str))))
              (if (member? sym variable-symbols)
                  (get-tokens-help (cdr input-list)
                                   variable-symbols
                                   nonterminal-symbols
                                   (append tokens
                                           (list "ID")))
                  (get-tokens-help (cdr input-list)
                                   variable-symbols
                                   nonterminal-symbols
                                   (append tokens
                                           (list (symbol->string sym)))))))
           ((car (contain-super-symbols? str))
            (let ((action-name (last (contain-super-symbols? str))))
              (get-tokens-help (cdr input-list)
                               variable-symbols
                               nonterminal-symbols
                               (append tokens (list (string-upcase action-name))))))
           ((equal? str "...")
            (let ((sym (gensym)))
              (set! gensym-list (append gensym-list (list sym)))
              (set! rec-list (append rec-list (list (if (list? (last tokens))
                                                        (map (lambda (str) (string->symbol str)) (last tokens))
                                                        (string->symbol (last tokens))))))
              (get-tokens-help (cdr input-list)
                               variable-symbols
                               nonterminal-symbols
                               (append (reverse (cdr (reverse tokens)))
                                       (list (symbol->string sym))))))
           (else
            (get-tokens-help (cdr input-list)
                             variable-symbols
                             nonterminal-symbols
                             (append tokens (list (string-upcase str)))))))))))

(define make-types
  (lambda (nonterminal-symbols fout)
    (cond
      ((null? nonterminal-symbols)
       '())
      (else
       (displayln (string-append "%type<Syntax."
                                 (symbol->string (car nonterminal-symbols))
                                 "> "
                                 (symbol->string (car nonterminal-symbols)))
                  fout)
       (make-types (cdr nonterminal-symbols) fout)))))

(define make-types-list
  (lambda (nonterminal-symbols fout)
    (cond
      ((null? nonterminal-symbols)
       '())
      (else
       (displayln (string-append "%type<Syntax."
                                 (symbol->string (car nonterminal-symbols))
                                 " list> "
                                 (symbol->string (car nonterminal-symbols)))
                  fout)
       (make-types-list (cdr nonterminal-symbols) fout)))))

; productions = (cddr data)
(define make-grammar
  (lambda (productions variant-name-list variable-symbols nonterminal-symbols fout)
    (cond
      ((null? productions)
       '())
      (else
       (let* ((production (car productions))
              (nonterminal-symbol (car production)))
         (cond
           ;  変数なら生成規則は不要
           ((member? nonterminal-symbol variable-symbols)
            (make-grammar (cdr productions)
                          (cdr variant-name-list)
                          variable-symbols
                          nonterminal-symbols
                          fout))
           (else
            (begin
;              (displayln (string-append (symbol->string nonterminal-symbol) " :") fout)
              (displayln (string-append (symbol->string nonterminal-symbol) " ") fout)
              (make-rhs (cdr production)
                        (car variant-name-list)
                        variable-symbols
                        nonterminal-symbols
                        fout ":")
              (make-grammar (cdr productions)
                            (cdr variant-name-list)
                            variable-symbols
                            nonterminal-symbols
                            fout)))))))))

; ...に未対応
(define make-rhs
  (lambda (rhs-list variant-names variable-symbols nonterminal-symbols fout mark)
    (cond
      ((null? rhs-list)
       (newline fout))
      ((null? variant-names)
       (newline fout))
      (else
       (let ((rhs (car rhs-list)))
         (cond
           ((equal? rhs 'boolean)
            (set! boolean-variant (car variant-names))
;            (displayln (string-append "\t| b\t{ "
            (displayln (string-append "\t" mark "  b\t{ "
                                      (car variant-names)
                                      " $1 }")
                       fout)
            (make-rhs (cdr rhs-list)
                      (cdr variant-names)
                      variable-symbols
                      nonterminal-symbols
                      fout "|"))
           ((symbol? rhs)
            (let ((token (car (get-tokens (list rhs)
                                          variable-symbols
                                          nonterminal-symbols))))
;              (displayln (string-append "\t| "
              (displayln (string-append "\t" mark " "
                                        token
                                        " { "
                                        (car variant-names) " "
                                        (make-attribute (string->symbol token) nonterminal-symbols 1 '())
                                        " }")
                         fout)
              (make-rhs (cdr rhs-list)
                        (cdr variant-names)
                        variable-symbols
                        nonterminal-symbols
                        fout "|")))
           ((list? rhs)
            (let ((tokens (make-list-tokens rhs
                                            variable-symbols
                                            nonterminal-symbols
                                            '())))
              (displayln (string-append "\t" mark " "
                                        (string-append* (cdr (append* (map (lambda (x) (list " " x)) tokens))))
                                        "\t{ "
                                        (car variant-names) " "
                                        (make-attribute (map (lambda (x) (string->symbol x)) tokens) nonterminal-symbols 1 '())
                                        " }")
                         fout)
              (make-rhs (cdr rhs-list)
                        (cdr variant-names)
                        variable-symbols
                        nonterminal-symbols
                        fout "|")))))))))

;...を使う場合にも対応
(define make-list-tokens
  (lambda (rhs variable-symbols nonterminal-symbols result)
    (cond
      [(null? rhs)
       result]
      ;(a b c) ..., a ...の場合
      [(and (not (null? (cdr rhs)))
            (equal? (second rhs) '...))
       ;rhsの先頭が...の対象でリストなら両端のカッコを外す
       (let ([sym (gensym)]
             [rec-rhs (make-list-tokens (if (symbol? (car rhs))
                                            (list (car rhs))
                                            (car rhs))
                                        variable-symbols
                                        nonterminal-symbols
                                        '())])
         (set! gensym-list (append gensym-list (list sym)))
         (set! rec-list (append rec-list (list (map (lambda (str) (string->symbol str)) rec-rhs))))
         (make-list-tokens (cddr rhs) variable-symbols nonterminal-symbols (append result (list (symbol->string sym)))))]
      [(list? (car rhs))
       (let ([tmp (append (make-list-tokens (car rhs) variable-symbols nonterminal-symbols '())
                          '("RPARE")
                          (cdr rhs))])
         (make-list-tokens tmp variable-symbols nonterminal-symbols (append result '("LPARE"))))]
      [(string? (car rhs))
       (make-list-tokens (cdr rhs) variable-symbols nonterminal-symbols (append result (list (car rhs))))]
      ;; symbolならtokenにする
      [else
       (make-list-tokens
        (cdr rhs)
        variable-symbols
        nonterminal-symbols
        (append result (get-tokens (list (car rhs)) variable-symbols nonterminal-symbols)))])))

(define make-attribute
  (lambda (tokens nonterminal-symbols  n index-list)
    (let ((literal '(NUM STR ID b)))  ;boolean用非終端記号のb
      (cond
        ((symbol? tokens)
         (if (or (member? tokens (append nonterminal-symbols literal))
                 (member? (symbol->string tokens) (map (lambda (sym) (symbol->string sym)) gensym-list)))
             (string-append "$" (number->string n))
             ""))
        ((null? tokens)
         (cond
           ((null? index-list)
            "")
           ((equal? (length index-list) 1)
            (string-append (string-append* (cdr (append* (map (lambda (x) (list ", " x)) index-list))))))
           (else
            (string-append "(" (string-append* (cdr (append* (map (lambda (x) (list ", " x)) index-list)))) ")"))))
        ((pair? tokens)
         (let ((token (car tokens)))
           (cond
             ((or (member? token (append nonterminal-symbols literal))
                  (member? (symbol->string token) (map (lambda (sym) (symbol->string sym)) gensym-list)))
              (make-attribute (cdr tokens)
                              nonterminal-symbols
                              (+ n 1)
                              (append index-list (list (string-append "$" (number->string n))))))
             (else
              (make-attribute (cdr tokens)
                              nonterminal-symbols
                              (+ n 1)
                              index-list)))))))))

(define append-gen
  (lambda (rec-lst gensym-lst lst)
    (cond
      ((null? gensym-lst)
       lst)
      (else
       (append-gen (cdr rec-lst)
                   (cdr gensym-lst)
                   (append lst (list (cons (car gensym-lst) (car rec-lst)))))))))

(define make-rec-grammar
  (lambda (productions nonterminal-symbols fout)
    (cond
      ((null? productions)
       '())
      (else
       (let* ((production (car productions))
              (lhs (car production)))
         (begin
           (make-rec-grammar-help production nonterminal-symbols fout)
           (make-rec-grammar (cdr productions)
                             nonterminal-symbols
                             fout)))))))

(define make-rec-grammar-help
  (lambda (production nonterminal-symbols fout)
    (let* ((lhs (car production))
           (rhs (cdr production))
           (rhs-tokens (string-append* (cdr (append* (map (lambda (x) (list " " x))
                                                          (map (lambda (sym) (symbol->string sym))
                                                               rhs)))))))
;      (displayln (string-append (symbol->string lhs) " :\n"
;                                "\t| { [] }")
      (displayln (string-append (symbol->string lhs) " \n"
                                "\t: { [] }")
                 fout)
      (display (string-append "\t| "
                              rhs-tokens
                              " "
                              (symbol->string lhs)
                              " { "
                              (up-first-char (symbol->string lhs)) " "
                              (make-attribute rhs nonterminal-symbols 1 '())
                              " :: "
                              (string-append "$" (number->string (+ 1 (length rhs))))
                              " }\n\n")
               fout))))

(define main
  (lambda ()
    (let* ((input-file (vector-ref (current-command-line-arguments) 0))
           (output-file (vector-ref (current-command-line-arguments) 1))
           (start-symbol-file (vector-ref (current-command-line-arguments) 2))
           (fin (open-input-file input-file))
           (fout (open-output-file output-file #:exists 'replace))
           (fout0 (open-output-file (string-append output-file "-part0") #:exists 'replace))
           (fout1 (open-output-file (string-append output-file "-part1") #:exists 'replace))
           (start-symbol-fin (open-input-file start-symbol-file))
           (data (read fin))
           (_ (close-input-port fin))
           (productions (cddr data))
           (boolean-flag (member? 'boolean (flatten productions)))
           (terminal-symbols (remove* '(...) (terminal-symbols-of data)))
           (variable-symbols (variable-symbols-of data))
           (nonterminal-symbols (remove* variable-symbols (nonterminal-symbols-of data)))
           (tokens (if boolean-flag
                       (remove "b" (get-tokens (append terminal-symbols '(TRUE FALSE LPARE RPARE EOF)) variable-symbols nonterminal-symbols))
                       (get-tokens (append terminal-symbols '(LPARE RPARE EOF)) variable-symbols nonterminal-symbols)))
           (start-symbol (read start-symbol-fin)))
      (begin
        (make-open-syntax fout0)
        (newline fout0)
        (make-num-id-string-token fout0)

        (display "%token " fout0)
        (displayln (string-append* (cdr (append* (map (lambda (x) (list " " x)) (remove-duplicates (remove* (list "NUM" "STR") tokens)))))) fout0)
        (newline fout0)

        (displayln "%start program" fout0)
        (displayln "%type<Syntax.program> program" fout0)
        (if boolean-flag
            (make-types (append nonterminal-symbols '(b)) fout0)
            (make-types nonterminal-symbols fout0))

        (displayln "%%" fout1)
        (newline fout1)
        (displayln (string-append "program :\n\t"
                                  (symbol->string start-symbol)
                                  " EOF { A_program $1 }")
                   fout1)
        (newline fout1)

        (make-grammar productions
                      (make-variant-name productions '())
                      variable-symbols
                      nonterminal-symbols
                      fout1)

        (if boolean-flag
;            (displayln (string-append "b :\n\t| TRUE { " "Boolean_b" " true }\n"
            (displayln (string-append "b \n\t:  TRUE { " "Boolean_b" " true }\n"
                                      "\t| FALSE { " "Boolean_b" " false }\n")
                       fout1)
            '())

        (make-rec-grammar (append-gen rec-list gensym-list '())
                          nonterminal-symbols
                          fout1)

        (make-types-list gensym-list fout0)
        (close-output-port fout0)

        (close-output-port fout1)

        (let* ([fin0 (open-input-file (string-append output-file "-part0"))]
               [fin1 (open-input-file (string-append output-file "-part1"))])
           (displayln (port->string fin0) fout)
           (displayln (port->string fin1) fout)
           (close-output-port fout))


        ))))

(main)
