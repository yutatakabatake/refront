#lang racket

(provide (all-defined-out))

; S式に変換できる文字列だけを変換する
(define string->sexpr
  (lambda (str)
    (with-input-from-string str
      (lambda ()
        (read)))))

; 文字列strにcharが含まれる回数
(define count-char
  (lambda (str char)
    (length (filter (lambda (c) (equal? c char)) (string->list str)))))

; esc-header : input-port -> #<procedure:language-info>
; ファイル読み込みでヘッダの情報を返す
(define esc-header
  (lambda (fin)
    (read-language fin)))

; member? : symbol × list -> boolean
; listにsymbolが出現するか判定する
(define member?
  (lambda (sym lst)
    (cond
      ((null? lst) #f)
      ((equal? sym (car lst)) #t)
      (else (member? sym (cdr lst))))))

; nonterminal-symbols-of : list -> list
; Redexで定義された構文 (define-language name (v ...) (e ...) (x ...)) から非終端記号(v e x)を取り出す
(define nonterminal-symbols-of
  (lambda (data)
    (let ((syntax (cddr data)))
      (letrec ((search-nonterminal (lambda (input ans-lst)
                                     (cond
                                       ((null? input)
                                        ans-lst)
                                       ((not (null? (car input)))
                                        (search-nonterminal (cdr input) (append ans-lst (list (caar input)))))))))
        (search-nonterminal syntax '())))))

; enumerate-symbols-recursive : list -> list
; symbolに番号をつける
(define enumerate-symbols-recursive
  (lambda (lst)
    (letrec ((helper
              (lambda (lst seen)
                (cond
                  ((null? lst) '())
                  (else
                   (let ((sym (car lst)))
                     (let ((count (+ 1 (length (filter (lambda (x) (symbol=? x sym)) seen)))))
                       (cons (string->symbol (string-append (symbol->string sym) (number->string count)))
                             (helper (cdr lst) (cons sym seen))))))))))
      (helper lst '()))))

; extract-grammar : input-port -> list
; eoplファイルからgrammarを抜き出す
(define extract-grammar
  (lambda (fin)
    (let ((lang-data (esc-header fin)))
      (letrec ((loop
                (lambda (data)
                  (cond
                    ((eof-object? data)
                     (close-input-port fin))
                    ((equal? (second data) 'grammar)
                     data)
                    (else
                     (loop (read fin)))))))
        (loop (read fin))))))

; terminal-symbols-of : list -> list
; (define-language lang-name ...)から終端記号のリストを返す
(define terminal-symbols-of
  (lambda (data)
    (let* ((nonterminal-symbols (nonterminal-symbols-of data))
           (grammar (cddr data))
           (all-vars (flatten grammar))
           (rsh-symbols (remove* nonterminal-symbols all-vars))
           (rem-vars (remove* '(variable variable-not-otherwise-mentioned ...) rsh-symbols)))
      (remove-duplicates (remove*
                          (filter (lambda (sym) (string-contains? (symbol->string sym) "_")) rem-vars)
                          rem-vars)))))

; contain-super-symbols? : string -> list
; 文字列がocamllexの動作名に使えない記号を含んでいるかを判定する
; 含む場合は (#t 記号を削除した文字列) を返す
; 含まない場合は (#f) を返す
(define contain-super-symbols?
  (lambda (str)
    (if
     (or (string-contains? str "/")
         (string-contains? str "!")
         (string-contains? str "?")
         (string-contains? str "-"))
     (list #t (remove-super-symbol str 0))
     '(#f))))

; symbol-index : string * number -> string
; 文字列から記号を削除する
(define remove-super-symbol
  (lambda (str n)
    (cond
      ((or (equal? (string (string-ref str n)) "/")
           (equal? (string (string-ref str n)) "!")
           (equal? (string (string-ref str n)) "?")
           (equal? (string (string-ref str n)) "-"))
       (string-append (substring str 0 n)
                      (substring str (+ 1 n))))
      (else
       (remove-super-symbol str (+ n 1))))))

; variable-symbols-of : list -> list
; (define-language lang-name ...)から変数を表す非終端記号のリストを返す
(define variable-symbols-of
  (lambda (data)
    (letrec ((find-variable-symbol
              (lambda (syntax lst)
                (cond
                  ((null? syntax)
                   lst)
                  ((member? 'variable-not-otherwise-mentioned (car syntax))
                   (find-variable-symbol (cdr syntax) (cons (car (car syntax)) lst)))
                  (else
                   (find-variable-symbol (cdr syntax) lst))))))
      (find-variable-symbol (cddr data) '()))))

; up-first-char : string -> string
; 文字列の1文字目を大文字にする
(define up-first-char
  (lambda (str)
    (string-append
     (string-upcase (substring str 0 1))
     (substring str 1))))

; remove-underscore : symbol -> symbol
; 非終端記号を受け取ってアンダースコアを含むならアンダースコア以降を消す
; 含まないならそのまま返す
(define remove-underscore
  (lambda (sym)
    (if (string-contains? (symbol->string sym) "_")
        (string->symbol (car (string-split (symbol->string sym) "_")))
        sym)))

; ファイルの最後に書かれているS式を取り出す
(define get-last-data
  (lambda (fin)
    (letrec ((loop
              (lambda (data acc)
                (cond
                  ((eof-object? data)
                   acc)
                  (else
                   (loop (read fin) data))))))
      (loop (read fin) '()))))

; 受け取ったリストの中から#:domainの後ろのシンボルを取り出す
(define get-domain
  (lambda (data)
    (second (member '#:domain (flatten data)))))

; 評価文脈用の定義から簡約する非終端記号を得る
(define get-program-field
  (lambda (Ev-data nonterminal-symbols)
    (let ((p-rhs (cadr (cadddr Ev-data))))
      (if (symbol? p-rhs)
          (symbol->string p-rhs)
          (symbol->string
           (car (filter (lambda (elem)
                          (member? elem nonterminal-symbols))
                        p-rhs)))))))

; バリアント名を生成する
; make-variant-name : list * list -> ListOf(String)
(define make-variant-name
  (lambda (productions result)
    (cond
      ((null? productions)
       result)
      (else
       (make-variant-name (cdr productions)
                          (append result
                                  (list (make-variant-name-help
                                         (caar productions)
                                         (cdar productions)
                                         '()))))))))

(define make-variant-name-help
  (lambda (lhs-symbol rhs-list result)
    (cond
      ((null? rhs-list)
       result)
      (else
       (let ((top-rhs (car rhs-list)))
         (cond
           ((equal? top-rhs '+)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Plus" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '-)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Minus" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '*)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Mult" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '/)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Div" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '<)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Lt" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '>)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Gt" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '<=)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Le" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '>=)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Ge" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '=)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Eq" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '!=)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Neq" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '==)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Eqeq" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '^)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Caret" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '<-)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Larrow" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs '->)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Rarrow" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((equal? top-rhs 'variable-not-otherwise-mentioned)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             result))
           ((or (equal? top-rhs 'number)
                (equal? top-rhs 'integer)
                (equal? top-rhs 'nutural)
                (equal? top-rhs 'real))
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (list (string-append "Num" (number->string (random 100)) "_" (symbol->string lhs-symbol))))))
           ((symbol? top-rhs)
            (if (car (contain-super-symbols? (symbol->string top-rhs)))
                (make-variant-name-help
                 lhs-symbol
                 (cdr rhs-list)
                 (append result
                         (list (string-append (up-first-char (last (contain-super-symbols? (symbol->string top-rhs))))
                                              (number->string (random 100)) "_"
                                              (symbol->string lhs-symbol)))))
                (make-variant-name-help
                 lhs-symbol
                 (cdr rhs-list)
                 (append result
                         (list (string-append (up-first-char (symbol->string top-rhs))
                                              (number->string (random 100)) "_"
                                              (symbol->string lhs-symbol)))))))
           ((list? top-rhs)
            (make-variant-name-help
             lhs-symbol
             (cdr rhs-list)
             (append result
                     (make-variant-name-help
                      lhs-symbol
                      (list (car (flatten top-rhs)))
                      '()))))))))))

; 開始記号から辿れる非終端記号の生成規則だけの表層言語文法を取得する
; get-surface-language : list * symbol -> list
(define get-surface-language
  (lambda (lang start-symbol)
    (append (list (first lang) (second lang)) (get-surface-language-help (cddr lang) start-symbol))))

(define get-surface-language-help
  (lambda (data start-symbol)
    (let* ([start-production (assoc start-symbol data)]
           [rhs-list (cdr start-production)]
           [lhs-list (map car data)])
      (letrec ([loop (lambda (rhs-list lhs-list result)
                       (let* ([follow-list (remove start-symbol
                                                   (get-follow-symbols rhs-list lhs-list '()))])
                         (cond
                           [(null? follow-list)
                            result]
                           [else
                            (loop (foldl (lambda (x result) (append result (cdr (assoc x data))))
                                         '()
                                         follow-list)
                                  (remove* follow-list lhs-list)
                                  (append-prods data follow-list result))])))])
        (loop rhs-list lhs-list (list start-production))))))

; rhs-list中のlhs-listに入っている記号を検知する
(define get-follow-symbols
  (lambda (rhs-list lhs-list result)
    (cond
      [(null? rhs-list) (remove-duplicates result)]
      [else
       (let ([top-rhs (car rhs-list)])
         (cond
           [(symbol? top-rhs)
            (if (member? top-rhs lhs-list)
                (get-follow-symbols (cdr rhs-list) lhs-list (append result (list top-rhs)))
                (get-follow-symbols (cdr rhs-list) lhs-list result))]
           [(list? top-rhs)
            (get-follow-symbols (append top-rhs (cdr rhs-list)) lhs-list result)]))])))

(define append-prods
  (lambda (data follow-list result)
    (cond
      [(null? follow-list) result]
      [else
       (let* ([follow-symbol (car follow-list)]
              [production (assoc follow-symbol data)])
         (append-prods data (cdr follow-list) (append result (list production))))])))

; ocamlyacc-reader : input-port -> list
; ocamlyacc用ファイルを読み込む
(define ocamlyacc-reader
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
                     (if (equal? (car input-list) 'program)
                         (append result (rest-reader fin (read-line fin) '()))
                         (loop fin (read-line fin) result))))))))
      (loop fin (read-line fin) '()))))

(define rest-reader
  (lambda (fin input result)
    (cond
      ((eof-object? input)
       result)
      (else
       (rest-reader fin
                    (read-line fin)
                    (append result
                            (list (map (lambda (str) (string->symbol str))
                                       (string-split input)))))))))
