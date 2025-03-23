#lang racket
(require "common.rkt")

; ocamlyaccが出力したASTをS式の表現に変換する変換器を生成する

(define syntax-reader
  (lambda (fin)
    (letrec ((loop
              (lambda (fin input result)
                (cond
                  ((eof-object? input)
                   result)
                  ((equal? input "")
                   (loop fin (read-line fin) result))
                  (else
                   (let ((input-list (map (lambda (str) (string->symbol str))
                                          (string-split input))))
                     (cond
                       ((> (length input-list) 2)
                        (if (equal? (second input-list) 'program)
                            (append result (rest-reader fin (read-line fin) '()))
                            (loop fin (read-line fin) result)))
                       (else
                        (loop fin (read-line fin) result)))))))))
      (loop fin (read-line fin) '()))))


(define get-program-field
  (lambda (fin)
    (letrec ([loop
              (lambda (input fin)
                (cond
                  ((eof-object? input)
                   '())
                  (else
                   (let ([input-list (map (lambda (str) (string->symbol str))
                                          (string-split input))])
                     (cond
                       ((null? input-list)
                        (loop (read-line fin) fin))
                       ((equal? (second input-list) 'program)
                        (last input-list))
                       (else
                        (loop (read-line fin) fin)))))))])
      (loop (read-line fin) fin))))

(define get-datatype
  (lambda (syntax-data out)
    (let ((line-lst (car syntax-data)))
      (cond
        ; 改行（空リスト）があったら一つの非終端記号に対するデータ型としてまとめる
        ((null? line-lst)
         (append
          out
          (list (cdr syntax-data))))
        ; データ型の宣言をする行は and v = のように書かれている
        ((equal? (car line-lst) 'and)
         (get-datatype (cdr syntax-data)
                       (append out (list (second line-lst)))))
        (else
         (get-datatype (cdr syntax-data)
                       (append out (list line-lst))))))))

(define get-prod
  (lambda (ocamlyacc-data out)
    (let ((line-lst (car ocamlyacc-data)))
      (cond
        ((null? line-lst)
         (append out
                 (list (cdr ocamlyacc-data))))
;        ((not (equal? (symbol->string (car line-lst)) "|"))
        ((not (or 
            (equal? (symbol->string (car line-lst)) "|")
            (equal? (symbol->string (car line-lst)) ":")))
         (get-prod (cdr ocamlyacc-data)
                   (append out (list (car line-lst)))))
        (else
         (get-prod (cdr ocamlyacc-data)
                   (append out (list line-lst))))))))

(define collect
  (lambda (result rest-data fun)
    (cond
      ((null? rest-data)
       result)
      (else
       (let ((tmp (fun rest-data '())))
         (collect (append result (list (reverse (cdr (reverse tmp)))))
                  (last tmp)
                  fun))))))

(define make-ast-to-sexpr
  (lambda (datatypes prods nonterminal-symbols lang-data key fout)
    (cond
      ((null? datatypes)
       '())
      (else
       (let* ((datatype (car datatypes))
              (lhs (car datatype))
              (prod (car prods))
              (lang-rhs-list (if (boolean? (car lang-data))
                                 '() ; arbnoの要素となる場合は空リスト
                                 (cdr (car lang-data)))))
         (displayln (string-append "and "
                                   (symbol->string lhs)
                                   "_to_sexpr "
                                   (symbol->string lhs)
                                   " =\n"
                                   (make-pattern-match (symbol->string lhs)
                                                       (cdr datatype)
                                                       (cdr prod)
                                                       ""
                                                       nonterminal-symbols
                                                       lang-rhs-list
                                                       (if (null? (cdr lang-data))
                                                           '()
                                                           (car (cdr lang-data)))
                                                       key))
                    fout)
         (make-ast-to-sexpr (cdr datatypes)
                            (cdr prods)
                            nonterminal-symbols
                            (cdr lang-data)
                            key
                            fout))))))

(define make-pattern-match
  (lambda (lhs variants prod out nonterminal-symbols lang-rhs-list next-lang-rhs-list key)
    (cond
      ((null? variants)
       (string-append "match " lhs  " with\n" out))
      ((equal? lhs "b") ;boolean用の非終端記号を作る
       "match b with\n\tBoolean_b bool1 -> if bool1 then \" #t \" else \" #f \"")
      (else
       (let ((rhs (if (member? '|[]| (flatten prod))
                      (reverse (cddr (reverse ; 先頭\を消す
                                      ; {　より後ろの要素と最後の自身の非終端記号を消す
                                      (cdr (reverse (member '\{ (reverse (second prod))))))))
                      (reverse (cdr (reverse ; 先頭\を消す
                                     ; {　より後ろの要素を消す
                                     (cdr (reverse (member '\{ (reverse (car prod))))))))))
             (variant (car variants)))
         (cond
           ((null? lang-rhs-list)
            (make-pattern-match lhs
                                (cdr variants)
                                (cdr prod)
                                (string-append out
                                               "\t| "
                                               (symbol->string (second variant))
                                               ;  ...の中身の変換部分
                                               (cond
                                                 [(equal? (length variant) 4) ; フィールドが1個
                                                  (let ([dot-prev (find-prev-before-dots next-lang-rhs-list)])
                                                    (cond
                                                      [(symbol? dot-prev) ; ...の前がシンボルならカッコをつけない
                                                       (string-append " " (symbol->string (last variant)) "1"
                                                                      " :: t "
                                                                      " -> "
                                                                      "\" \""
                                                                      (make-strings rhs nonterminal-symbols (enumerate-symbols-recursive (cdddr variant)) key "")
                                                                      " ^ "
                                                                      lhs "_to_sexpr t\n"
                                                                      "\t| [] -> \"\"\n")
                                                       ]
                                                      [(list? dot-prev) ; ...の前がリストならカッコをつける
                                                       (string-append " " (symbol->string (last variant)) "1"
                                                                      " :: t "
                                                                      " -> "
                                                                      "\"(\" "
                                                                      (make-strings rhs nonterminal-symbols (enumerate-symbols-recursive (cdddr variant)) key "")
                                                                      " ^ \")\" ^ "
                                                                      lhs "_to_sexpr t\n"
                                                                      "\t| [] -> \"\"\n")]))]
                                                 [else ; フィールドが複数
                                                  ;  ...の前がリストの場合とシンボルの場合
                                                  ; 次のlang-rhs-list (car (cdr lang-rhs-list))から...を探して直前の要素がリストかシンボルかを判定する
                                                  (let ([dot-prev (find-prev-before-dots next-lang-rhs-list)])
                                                    (cond
                                                      [(symbol? dot-prev)
                                                       (string-append "("
                                                                      (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                                                                                         (map (lambda (sym) (symbol->string sym))
                                                                                                              (enumerate-symbols-recursive
                                                                                                               (filter
                                                                                                                (lambda (sym) (not (equal? sym 'list)))
                                                                                                                (remove* '(*) (cdddr variant)))))))))
                                                                      ") :: t "
                                                                      " -> "
                                                                      "\" \""
                                                                      (make-strings rhs
                                                                                    nonterminal-symbols
                                                                                    (enumerate-symbols-recursive (remove* '(*) (cdddr variant)))
                                                                                    key
                                                                                    "")
                                                                      " ^ "
                                                                      lhs "_to_sexpr t\n"
                                                                      "\t| [] -> \"\"\n")]
                                                      [(list? dot-prev)
                                                       (string-append "("
                                                                      (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                                                                                         (map (lambda (sym) (symbol->string sym))
                                                                                                              (enumerate-symbols-recursive
                                                                                                               (filter
                                                                                                                (lambda (sym) (not (equal? sym 'list)))
                                                                                                                (remove* '(*) (cdddr variant)))))))))
                                                                      ") :: t "
                                                                      " -> "
                                                                      "\"(\""
                                                                      (make-strings rhs
                                                                                    nonterminal-symbols
                                                                                    (enumerate-symbols-recursive (remove* '(*) (cdddr variant)))
                                                                                    key
                                                                                    "")
                                                                      " ^ \")\" ^ "
                                                                      lhs "_to_sexpr t\n"
                                                                      "\t| [] -> \"\"\n")]))]))
                                nonterminal-symbols
                                '()
                                next-lang-rhs-list
                                key))
           ((equal? (car lang-rhs-list) 'boolean) ; booleanをredexで使っていた場合
            (make-pattern-match lhs
                                (cdr variants)
                                (cdr prod)
                                (string-append out
                                               "\t| "
                                               (symbol->string (second variant))
                                               " "
                                               (symbol->string (fourth variant)) "1"
                                               " -> "
                                               "b_to_sexpr b1\n")
                                nonterminal-symbols
                                (cdr lang-rhs-list)
                                next-lang-rhs-list
                                key))
           ((symbol? (car lang-rhs-list))
            (make-pattern-match lhs
                                (cdr variants)
                                (cdr prod)
                                (string-append out
                                               "\t| "
                                               (symbol->string (second variant))
                                               " "
                                               (cond
                                                 ((equal? (length variant) 2) ; フィールドが0個
                                                  (string-append " -> "
                                                                 "\" \""
                                                                 (make-strings rhs nonterminal-symbols '() key "")
                                                                 "\n"))
                                                 ((equal? (length variant) 4) ; フィールドが1個
                                                  (string-append (symbol->string (last variant)) "1"
                                                                 " -> "
                                                                 "\" \""
                                                                 (make-strings rhs nonterminal-symbols (enumerate-symbols-recursive (cdddr variant)) key "")
                                                                 "\n"))))
                                nonterminal-symbols
                                (cdr lang-rhs-list)
                                next-lang-rhs-list
                                key))
           ; 両端にカッコをつける
           ((list? (car lang-rhs-list))
            (make-pattern-match lhs
                                (cdr variants)
                                (cdr prod)
                                (string-append out
                                               "\t| "
                                               (symbol->string (second variant))
                                               " "
                                               (cond
                                                 ((equal? (length variant) 2) ; フィールドが0個
                                                  (string-append " -> "
                                                                 "\"(\" "
                                                                 (make-strings rhs nonterminal-symbols '() key "")
                                                                 " ^ \")\"\n"))
                                                 ((equal? (length variant) 4) ; フィールドが1個
                                                  (string-append (symbol->string (last variant)) "1"
                                                                 " -> "
                                                                 "\"(\" "
                                                                 (make-strings rhs
                                                                               nonterminal-symbols
                                                                               (enumerate-symbols-recursive (cdddr variant))
                                                                               key
                                                                               "")
                                                                 " ^ \")\"\n"))
                                                 (else ; フィールドが複数
                                                  (string-append "("
                                                                 (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                                                                                    (map (lambda (sym) (symbol->string sym))
                                                                                                         (enumerate-symbols-recursive
                                                                                                          (filter
                                                                                                           (lambda (sym) (not (equal? sym 'list)))
                                                                                                           (remove* '(*) (cdddr variant))))))))) ;ofより後ろのfieldを取り出す
                                                                 ")"
                                                                 " -> "
                                                                 "\"(\""
                                                                 (make-strings rhs
                                                                               nonterminal-symbols
                                                                               (enumerate-symbols-recursive (remove* '(* list) (cdddr variant)))
                                                                               key
                                                                               "")
                                                                 " ^ \")\" \n"))))
                                nonterminal-symbols
                                (cdr lang-rhs-list)
                                next-lang-rhs-list
                                key))))))))

(define make-strings
  (lambda (rhs nonterminal-symbols enumerated-symbols key out)
    (cond
      ((null? rhs)
       out)
      ((equal? (car rhs) 'PLUS)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" + \"")))
      ((equal? (car rhs) 'MINUS)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" - \"")))
      ((equal? (car rhs) 'MULT)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" * \"")))
      ((equal? (car rhs) 'DIV)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" / \"")))
      ((equal? (car rhs) 'LT)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" < \"")))
      ((equal? (car rhs) 'GT)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" > \"")))
      ((equal? (car rhs) 'LE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" <= \"")))
      ((equal? (car rhs) 'GE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" >= \"")))
      ((equal? (car rhs) 'EQ)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" = \"")))
      ((equal? (car rhs) 'EQEQ)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" == \"")))
      ((equal? (car rhs) 'NEQ)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" != \"")))
      ((equal? (car rhs) 'CARET)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" ^ \"")))
      ((equal? (car rhs) 'RARROW)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" -> \"")))
      ((equal? (car rhs) 'LARROW)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" <- \"")))
      ((equal? (car rhs) 'LBRACE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" l-brace \"")))
      ((equal? (car rhs) 'RBRACE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" r-brace \"")))
      ((equal? (car rhs) 'LBRACKET)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" l-bracket \"")))
      ((equal? (car rhs) 'RBRACKET)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" r-bracket \"")))
      ((equal? (car rhs) 'LPARE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" ( \"")))
      ((equal? (car rhs) 'RPARE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" ) \"")))
      ; lexerと連携させるのが理想
      ((equal? (car rhs) 'CALLCC)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ \" call/cc \"")))
      ((equal? (car rhs) 'L)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ \" L \"")))
      ((equal? (car rhs) 'A)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ \" A \"")))
      ((equal? (car rhs) 'NUM)
       (if (null? enumerated-symbols)
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         enumerated-symbols
                         key
                         (string-append out " ^ " "string_of_int int"))
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         (cdr enumerated-symbols)
                         key
                         (string-append out " ^ " "string_of_int " (symbol->string (car enumerated-symbols))))))
      ((equal? (car rhs) 'ID)
       (if (null? enumerated-symbols)
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         enumerated-symbols
                         key
                         (string-append out " ^ " "id"))
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         (cdr enumerated-symbols)
                         key
                         (string-append out " " " ^ " (symbol->string (car enumerated-symbols))))))
      ((equal? (car rhs) 'TRUE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" #t \"")))
      ((equal? (car rhs) 'FALSE)
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " ^ " "\" #f \"")))
      ((equal? (car rhs) 'STR)
       (if (null? enumerated-symbols)
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         enumerated-symbols
                         key
                         (string-append out " ^ " "string"))
           (make-strings (cdr rhs)
                         nonterminal-symbols
                         (cdr enumerated-symbols)
                         key
                         (string-append out " " " ^ " (symbol->string (car enumerated-symbols))))))
      ((member? (car rhs) (append nonterminal-symbols key))
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     (cdr enumerated-symbols)
                     key
                     (string-append out " " " ^ "  (symbol->string (car rhs)) "_to_sexpr " (symbol->string (car enumerated-symbols)))))
      (else
       (make-strings (cdr rhs)
                     nonterminal-symbols
                     enumerated-symbols
                     key
                     (string-append out " " " ^ " "\" " (string-downcase (symbol->string (car rhs))) " \""))))))

(define rearrange
  (lambda  (list1 list2)
    (let ((key-order (map car list1)))
      (map (lambda (key)
             (assoc key list2)) key-order))))

; ...の前の要素を取り出す
(define find-prev-before-dots
  (lambda (lst)
    (helper '() lst)))

(define helper
  (lambda (prev lst)
    (cond
      [(empty? lst) '()] ; リストが空なら結果なし
      [(list? (first lst)) ; 現在の要素がリストの場合
       (let ((result (helper prev (first lst)))) ; 再帰的に内側のリストを探索
         (if (not (eq? result '())) result (helper (first lst) (rest lst))))] ; 内側のリストに '... がなければ続ける
      [(eq? (first lst) '...) prev] ; '... が見つかったらその直前の要素を返す
      [else (helper (first lst) (rest lst))]))) ; リストが単純な要素なら次に進む


(define main
  (lambda ()
    (let* ((merged-lang-file (vector-ref (current-command-line-arguments) 0))
           (ocamlyacc-file (vector-ref (current-command-line-arguments) 1))
           (syntax-file (vector-ref (current-command-line-arguments) 2))
           (program-field-file (vector-ref (current-command-line-arguments) 2))
           (output-file (vector-ref (current-command-line-arguments) 3))
           (merged-fin (open-input-file merged-lang-file))
           (ocamlyacc-fin (open-input-file ocamlyacc-file))
           (syntax-fin (open-input-file syntax-file))
           (program-field-fin (open-input-file program-field-file))
           (fout (open-output-file output-file #:exists 'replace))

           (lang-data (read merged-fin))
           (nonterminal-symbols (nonterminal-symbols-of lang-data))
           (ocamlyacc-data (append (cddr (ocamlyacc-reader ocamlyacc-fin)) (list '())))
           (syntax-data (syntax-reader syntax-fin))
           (datatypes (collect '() syntax-data get-datatype))
           (productions (rearrange datatypes (remove* '(()) (collect '() ocamlyacc-data get-prod))))
           (rearranged-lang-data (rearrange datatypes (cddr lang-data)))
           (program-field (symbol->string (get-program-field program-field-fin)))
           (key (map car datatypes))
           (_ (close-input-port merged-fin))
           (_ (close-input-port ocamlyacc-fin))
           (_ (close-input-port syntax-fin))
           (_ (close-input-port program-field-fin)))
      (displayln "open Syntax" fout)

      (displayln (string-append "let rec program_to_sexpr program =\n"
                                "\tmatch program with A_program "
                                program-field " -> " program-field "_to_sexpr " program-field)
                 fout)

      (make-ast-to-sexpr datatypes
                         productions
                         (if (member? 'b key)
                             nonterminal-symbols
                             (append nonterminal-symbols '(b)))
                         rearranged-lang-data
                         key
                         fout)

      (close-output-port fout))))

(main)
