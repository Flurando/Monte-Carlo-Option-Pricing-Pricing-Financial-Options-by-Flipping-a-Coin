#!/bin/sh
exec guile -L lib "$0"
!#

(set! *random-state* (random-state-from-platform))
(use-modules (plot)
	     (model)
	     (util))

(define s 45)
(define r 0.06)
(define delta 0.2)
(define t 5/12)
(define n 200)
(define k 50)
(define m 10000)

(define* (make-epsilon-lst #:optional (n n))
  (make-list-with-thunk n get-epsilon))

;; Implement as a 2n long list, so one-shot is just enough for a single run
;; This is because previous code was written without letting two get-epsilons to communicate
;; and I am reluctant for a rewrite
(define* (make-get-determined-epsilon #:optional (n n))
  (let ((current-lst (let ((lst (make-epsilon-lst n)))
		       (append lst (map-in-order (lambda (x) (- x)) lst)))))
    (lambda args
      (define answer (car current-lst))
      (set! current-lst (cdr current-lst))
      answer)))
	  
(define* (simulate-2m #:optional (s s) (m m))
  (define payoff (make-payoff k))
  (/ (let loop ((m m))
       (format #t "[2m] working on m = ~a~%" m)
       (if (zero? m)
	   0
	   (+ (let ((calculator (make-s-sequence-calculator delta r (/ t n) (make-get-determined-epsilon n))))
		(payoff (last-element (get-s-sequence-n  calculator n s)))
		(payoff (last-element (get-s-sequence-n  calculator n s))))
	      (loop (1- m)))))
     (* 2 m)))

(define* (simulate-round #:optional (s s) (m m) (round 20) #:key (simulator simulate-2m))
  (make-list-with-thunk round (lambda () (simulator s m))))

;; BEGIN copied from q3
(define payoff (make-payoff k))

(define* (simulate-m #:optional (s s) (m m))
  (/ (let loop ((m m))
       (format #t "[m] working on m = ~a~%" m)
       (if (zero? m)
	   0
	   (+ (payoff (last-element (get-s-sequence-n (make-s-sequence-calculator delta r (/ t n)) n s)))
	      (loop (1- m)))))
     m))
;; END

(define new-result (simulate-round s m 20))
(define old-result (simulate-round s m 20 #:simulator simulate-m))
(define (report msg lst)
  (format #t "~a~%for 20 rounds:~%~a~%mean: ~a~%standard-derivation: ~a~%" msg lst (mean lst) (standard-derivation lst)))
(report "new way using ({}+{})/2" new-result)
(report "old way using {}" old-result)
(display "Rearrange\nthe new method:\n")
(list->histogram new-result 7)
(display "the old method:\n")
(list->histogram old-result 7)
