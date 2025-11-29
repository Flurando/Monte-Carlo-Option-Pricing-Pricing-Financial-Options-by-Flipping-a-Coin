(define-module (plot)
  #:use-module (plotutils graph)
  #:use-module (model)
  #:use-module (util)
  #:export (simulate-n))

(define t (map-in-order (lambda (x) (exact->inexact (/ x 254))) (iota 255 0)))
(define (get-s) (get-s-sequence-n (make-s-sequence-calculator)))

(define* (simulate-n #:optional (n 5) (get-s get-s) (t t) #:key (title "") (output-filename "images/result.png"))
  (with-output-to-file output-filename
    (lambda ()
      (graph (apply merge (make-list n t))
	     (apply merge (make-list-with-thunk n get-s))
	     #:output-format "png"
	     #:top-label title))
    #:binary #t))

