;;; Sussman's Anomaly Domain with Types
;;; Vladimir Kulyukin

(define (domain sussman-anomaly-typed)
   (:requirements :strips :typing)
   (:types BLOCK TABLE)
   (:predicates
	(on ?x - BLOCK ?y - (either BLOCK TABLE))
	(clear ?x - BLOCK))

    (:action MOVE_01
	 :parameters (?b1 - BLOCK ?b2 - BLOCK ?t - TABLE)
	 :precondition (and (clear ?b1)
			    (on ?b1 ?b2)
			    (on ?b2 ?t))
         :effect (and (not (on ?b1 ?b2))
	 	      (on ?b1 ?t)
		      (clear ?b2)))

    (:action MOVE_02
	 :parameters (?b1 - BLOCK ?t - TABLE ?b2 - BLOCK)
	 :precondition (and (clear ?b1) 
	 	       	    (clear ?b2)
			    (on ?b1 ?t)
			    (on ?b2 ?t))
         :effect (and (on ?b1 ?b2)
	 	      (not (on ?b1 ?t))
		      (not (clear ?b2))))

    (:action MOVE_03
	 :parameters (?b1 - BLOCK ?t - TABLE ?b2 - BLOCK ?b3 - BLOCK)
	 :precondition (and (clear ?b1) 
	 	       	    (clear ?b2)
			    (on ?b1 ?t)
			    (on ?b2 ?b3)
			    (on ?b3 ?t))
         :effect (and (not (on ?b1 ?t))
	 	      (on ?b1 ?b2)
		      (not (clear ?b2))))

)
