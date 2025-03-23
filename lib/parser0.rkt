#lang racket

(provide parse)

(define read-program
  (lambda (fin acc)
   (let ((data-char (read-char fin)))
    (cond
      ((eof-object? data-char)
       acc)
      (else
       (read-program fin (string-append acc (string data-char))))))))

(define (load-module-from-string fname ast2list)
  (dynamic-require (string->path ast2list) fname))

(define parse
 (lambda
  (ast2list input-filename)
   (let*
    (
     [output-file (path-replace-extension input-filename ".sexp")]
     [fin (open-input-file input-filename)]
     [data (read-program fin "")]
     [fout (open-output-file output-file #:exists (quote replace))]
     [program-to-list (load-module-from-string 'program-to-list ast2list)]
     [s&p (load-module-from-string 's&p ast2list)])
    (begin
      (write (program-to-list (s&p data)) fout)
      (close-output-port fout)
      (close-input-port fin)))))
