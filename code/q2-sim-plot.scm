#!/bin/sh
exec guile -L lib "$0"
!#

(set! *random-state* (random-state-from-platform))
(use-modules (plot)
	     (model))
(simulate-n #:title "delta 0.1" #:output-filename "images/q2-delta-0.1.png")
(simulate-n 5 (lambda () (get-s-sequence-n (make-s-sequence-calculator 0.25)))
	    #:title "delta 0.25" #:output-filename "images/q2-delta-0.25.png")
