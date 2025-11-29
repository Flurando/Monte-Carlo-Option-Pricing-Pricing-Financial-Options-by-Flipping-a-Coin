#!/bin/sh
exec guile -L lib "$0"
!#

(set! *random-state* (random-state-from-platform))
(use-modules (plot)
	     (model)
	     (util))

(define s 45) ; 50, 55
(define r 0.06) ; the old mu
(define delta 0.2)
(define t 5/12)
(define n 200)
(define k 50)
(define m 10000)

(define payoff (make-payoff k))

(define* (simulate-m #:optional (s s) (m m))
  (/ (let loop ((m m))
       (if (zero? m)
	   0
	   (+ (payoff (last-element (get-s-sequence-n (make-s-sequence-calculator delta r (/ t n)) n s)))
	      (loop (1- m)))))
     m))

(define* (estimate #:optional (s s) (m m))
  (* (expt (+ 1 (* r (/ t n))) (- n))
     (simulate-m s m)))

(define (simulate s m)
  (format #t "Case s = ~a~%" s)
  (format #t "After ~a rounds~%" m)
  (define mc (estimate s m))
  (define bs (black-scholes delta t s k r))
  (format #t "Morte Carlo: ~a~%" mc)
  (display "Compared to Black Scholes formula result")
  (newline)
  (format #t "Black Scholes: ~a~%" bs)
  (format #t "The percentage error taking BSM as reference is: ~a%~%" (percentage-error mc bs)))

(simulate 45 m)
(simulate 50 m)
(simulate 55 m)
