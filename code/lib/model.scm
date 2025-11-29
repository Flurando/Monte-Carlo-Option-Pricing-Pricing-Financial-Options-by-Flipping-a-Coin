(define-module (model)
  #:export (get-epsilon make-s-sequence-calculator get-s-sequence-n))

(define s 40)
(define mu 0.12)
(define delta 0.1)
(define t 1)
(define n 254)

(define (get-epsilon . args)
  "equal randomly generate 1 or -1"
  (if (zero? (random 2))
      1 -1))

(define* (make-s-sequence-calculator #:optional (delta delta) (mu mu) (delta-t (/ t n)) (get-epsilon get-epsilon))
  (lambda (s-last)
    (+ s-last
       (* mu s-last delta-t)
       (* delta s-last (get-epsilon s-last)
          (sqrt delta-t)))))

(define* (get-s-sequence-n calculator #:optional (n n) (s s))
  (let loop
      ((index 0)
       (s-last s))
    (if (= index n)
	'()
	(let ((s-new (calculator s-last)))
          (cons s-new (loop (1+ index) s-new))))))


