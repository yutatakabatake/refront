#lang racket

(require redex)
(provide load-and-execute)

(define load-and-execute
  (lambda (s-exp-file langfile)
  (begin
    (let* ([run (load-module-from-string 'run langfile)]
           [fin (open-input-file s-exp-file)]
           [s-exp (read fin)])
      (begin
;           (run s-exp))))
        (display (run s-exp))
        (display "\n"))))))

(define (load-module-from-string fname langfile)
  (dynamic-require (string->path langfile) fname))
