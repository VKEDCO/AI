;;; Sussman Anomaly Problem
;;; Vladimir Kulyukin
(define (problem sa-problem)
	(:domain sussman-anomaly)
  	(:objects A B C T)
   	(:init 
	   (block A) (block B) (block C) (table T)
	   (on B T)
	   (on A T)
	   (on C A)
	   (clear B)
	   (clear C))
	(:goal (and (on A B) (on B C))))
