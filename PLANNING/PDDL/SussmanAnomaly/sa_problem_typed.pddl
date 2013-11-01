;;; Sussman's Anomaly Problem with Types
;;; Vladimir Kulyukin
(define (problem sussman-problem-typed)
  (:domain sussman-anomaly-typed)
  (:objects
	A - BLOCK
	B - BLOCK 
	C - BLOCK
	T - TABLE)
  (:init 
  	 (on B T) 
  	 (on A T) 
	 (on C A)
  	 (clear B) 
	 (clear C))
  (:goal (and 
  	      (on A B) 
	      (on B C))))
