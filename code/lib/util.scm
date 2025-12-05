(define-module (util)
  #:export (make-list-with-thunk black-scholes percentage-error make-payoff last-element mean standard-derivation list->histogram))

(define (make-list-with-thunk n thunk)
  (let loop ((n n))
    (if (zero? n)
	'()
	(cons (thunk)
	      (loop (1- n))))))

#!
# Source - https://stackoverflow.com/
# Posted by John D. Cook, modified by community. See post 'Timeline' for change history
# License - CC BY-SA 3.0
def erf(x):
# save the sign of x
sign = 1 if x >= 0 else -1
x = abs(x)

# constants
a1 =  0.254829592
a2 = -0.284496736
a3 =  1.421413741
a4 = -1.453152027
a5 =  1.061405429
p  =  0.3275911

# A&S formula 7.1.26
t = 1.0/(1.0 + p*x)
y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*math.exp(-x*x)
return sign*y # erf(-x) = -erf(x)
!#

(define erf
  (lambda (x)
    (define sign
      (if (>= x 0)
	  1
	  -1))
    (define abs-x (abs x))
    (define a1 0.254829592)
    (define a2 -0.284496736)
    (define a3 1.421413741)
    (define a4 -1.453152027)
    (define a5 1.061405429)
    (define p 0.3275911)

    (define t (/ 1.0 (+ 1.0 (* p abs-x))))
    (define y (- 1.0 (* t
			(exp (- (* abs-x abs-x)))
			(+ a1 (* t
				 (+ a2 (* t
					  (+ a3 (* t
						   (+ a4 (* t
							    a5)))))))))))
    (* sign y)))

(define (erfc x)
  (- 1 (erf x)))

(define (get-d1 delta t s k r)
  (* (/ 1 (* delta (sqrt t)))
     (+ (log (/ s k))
	(* t
	   (+ r (/ (expt delta 2) 2))))))
(define (get-d2 delta t s k r)
  (- (get-d1 delta t s k r)
     (* delta (sqrt t))))
(define (black-scholes delta t s k r)
  (- (* (/ s 2)
	(erfc (- (/ (get-d1 delta t s k r)
		    (sqrt 2)))))
     (* (/ k 2)
	(exp (- (* r t)))
	(erfc (- (/ (get-d2 delta t s k r)
		    (sqrt 2)))))))

(define (percentage-error current should)
  (* (/ (abs (- current should)) should) 100))


(define (make-payoff k)
  (lambda (s-current)
    (max (- s-current k) 0)))

(define (last-element lst)
  (car (last-pair lst)))

(define (mean lst)
  (/ (apply + lst)
     (length lst)))

(define (square x) (* x x))

(define (standard-derivation lst)
  (sqrt (mean (map (lambda (x) (square (- x (mean lst))))
		   lst))))

(define* (list->histogram lst #:optional (slices 5))
  (define maximum (apply max lst))
  (define minimum (apply min lst))
  (define step (/ (- maximum minimum) slices))
  (define (make-decider n) (lambda (x) (and (>= x (+ minimum (* n step)))
					    (< x (+ minimum (* (1+ n) step))))))
  (define decider-list (map-in-order make-decider (iota slices 0 1)))
  (define holder (make-list (1+ slices) '()))
  (set! holder
	(let loop ((lst lst))
	  (if (null? lst)
	      holder
	      (begin
		(let ((index (let inner-loop ((deciders decider-list))
			       (if (null? deciders)
				   0
				   (if ((car deciders) (car lst))
				       0
				       (1+ (inner-loop (cdr deciders))))))))
		  (list-set! holder
			     index
			     (cons (car lst) (list-ref holder index))))
		(loop (cdr lst))))))
  (define holder-length-list (map-in-order (lambda (x) (length x)) holder))
  (let loop ((n 0))
    (unless (= n slices)
      (format #t "~a<={}<~a: ~a~%" (+ minimum (* n step)) (+ minimum (* (1+ n) step)) (list-ref holder-length-list n))
      (loop (1+ n))))
  (format #t "outage: ~a~%" (list-ref holder-length-list slices))
  (define holder-length-percentage-integer
    (let ((total (apply + holder-length-list)))
      (map-in-order (lambda (x) (floor/ (* 100 x) total)) holder-length-list)))
  (display "Histogram:\n")
  (let loop ((n 0))
    (unless (= n slices)
      (format #t "~a<={}<~a~%| ~a~%" (+ minimum (* n step)) (+ minimum (* (1+ n) step)) (make-string (list-ref holder-length-percentage-integer n) #\*))
      (loop (1+ n))))
  (format #t "outage:~%| ~a~%" (make-string (list-ref holder-length-percentage-integer slices) #\*)))
