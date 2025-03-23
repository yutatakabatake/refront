#lang racket

(define check-ocamlyacc
  (lambda ()
    (let ([ocamlyacc-path (vector-ref (current-command-line-arguments) 0)]
          [ocamlyacc-file (vector-ref (current-command-line-arguments) 1)])
      (begin
        (define-values (sp out in err)
;          (subprocess #f #f #f ocamlyacc-path "parser.mly"))
;          (subprocess #f #f #f ocamlyacc-path ocamlyacc-file))
          (subprocess #f #f #f #f ocamlyacc-path ocamlyacc-file "-o/dev/null"))
        (let ([message (port->string err)])
          (if (string-contains? message "reduce/reduce")
              ;   reduce/reduceならメッセージを出して停止
              (begin
                (displayln message)
                (displayln "stop via reduce/reduce")
                ; (error "stop via reduce/reduce")
                (exit 1))
              (displayln "OK grammar")))
        (close-input-port out)
        (close-output-port in)
        (close-input-port err)
        (subprocess-wait sp)))))

(check-ocamlyacc)
