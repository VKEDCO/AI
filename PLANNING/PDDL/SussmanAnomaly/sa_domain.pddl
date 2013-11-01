;;; Sussman's Anomaly
;;; Vladimir Kulyukin
(define (domain sussman-anomaly)
   (:requirements :strips :equality)
   (:predicates
	(block ?x)
	(on ?x ?y)
	(clear ?x)
	(table ?x))

(:action MOVE_01
	 :parameters (?b1 ?b2 ?t)
	 :precondition (and (block ?b1) (block ?b2)
	 	            (table ?t)
	 	       	    (clear ?b1)
			    (on ?b1 ?b2)
			    (on ?b2 ?t))
         :effect (and (not (on ?b1 ?b2))
	 	      (on ?b1 ?t)
		      (clear ?b2)))

(:action MOVE_02
	 :parameters (?b1 ?t ?b2)
	 :precondition (and (block ?b1) (block ?b2)
	 	       	    (table ?t)
			    (clear ?b1) (clear ?b2)
			    (on ?b1 ?t)
			    (on ?b2 ?t))
         :effect (and (on ?b1 ?b2)
	 	      (not (on ?b1 ?t))
		      (not (clear ?b2))))

(:action MOVE_03
	 :parameters (?b1 ?t ?b2 ?b3)
	 :precondition (and (block ?b1) (block ?b2) (block ?b3)
	 	            (table ?t)
	 	       	    (clear ?b1) (clear ?b2)
			    (on ?b1 ?t)
			    (on ?b2 ?b3)
			    (on ?b3 ?t))
         :effect (and (not (on ?b1 ?t))
	 	      (on ?b1 ?b2)
		      (not (clear ?b2))))

)
