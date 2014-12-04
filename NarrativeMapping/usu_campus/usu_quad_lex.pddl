;;; USU Main Quad
;;; author: Vladimir Kulyukin
;;; (:goal (and (lexicalized-agent-location Agent01 OldMain)
;;;             (lexicalized-agent-direction Agent01 East)
;;;             (lexicalized-travel-command Agent01 OldMain MiddleOfQuad East)
;;;             (lexicalized-agent-location Agent01 MiddleOfQuad)
;;;             (lexicalized-agent-direction Agent01 East)
;;;             (lexicalized-agent-turn-command Agent01 MiddleOfQuad East North)
;;;             (lexicalized-agent-travel-command Agent01 MiddleOfQuad AnimalSciences)
;;;             (lexicalized-agent-location Agent01 AnimalSciences)
;;;             (lexicalized-agent-direction Agent01 North)))
;;; (agent ?a)
;;; (building ?b)
;;; (place ?p)
;;; (direction ?dir)
;;; (:action LEXICALIZE-TRAVEL-COMMAND-FROM-PLACE-TO-BUILDING
;;; 	     :parameters (?a ?b ?p ?dir)
;;;	     :precondition (and (agent ?a) (building ?b) (place ?p)
;;;	     		   (direction ?dir))
;;;	     :effect (and (lexicalized-travel-command-from-place-to-building 
;;;	     	     	    ?a ?b ?p ?dir)))
;;; (:action LEXICALIZE-TRAVEL-ADVICE-FROM-PLACE-TO-BUILDING ....)
;;; (:action LEXICALIZE-TURN-COMMAND-AT-PLACE ...)
;;; (:action LEXICALIZE-TURN-ADVICE-AT-PLACE ...)
;;; (:action LEXICALIZE-DIRECTION ...)
;;; (:predicates (agent ?a) (building ?b) (place ?p) (direction ?dir)
;;; 		 (travel-action ?a) (turn-action ?a)

(define (domain usu-main-quad-lexicalization)
   (:requirements :strips :equality)
   (:predicates
	(building ?b)		;;; ?b is a building
	(direction ?d) 		;;; ?d is a direction (east, west, north, south)
	(agent ?a)     	      	;;; ?a is an agent
	(place ?p) 	      	;;; ?p is a place (places are dimensionless)
	(turn-action ?a)	;;; ?a is a turn
	(travel-action ?a)	;;; ?a is a travel
	(agent-of ?action ?agent) ;;; agent ?agent is the agent of action ?action
	(lexicalized-agent-location ?a ?loc) ;;; agent ?a's location at ?loc is lexicalized
	(lexicalized-travel-command ?agent ?action ?from ?to ?dir)
	(lexicalized-travel-advice ?agent ?action ?from ?to ?dir)
	(lexicalized-turn-command ?agent ?action ?place ?old-dir ?new-dir)
	(lexicalized-turn-advice ?agent ?action ?place ?old-dir ?new-dir)
	(route ?r)		    ;;; ?r is a route
	(action-of ?r ?a)	    ;;; ?a is an action of ?r
	(start-of ?r ?s)	    ;;; ?s is the start of route ?r; ?s is a place or building
	(end-of ?r ?s)
	(before ?a1 ?a2 ?r)	    ;;; action ?a1 occurs before action ?a2 on route ?r
	(at ?agent ?loc)
	(facing ?agent ?dir)
	)

   (:action LEXICALIZE-AGENT-LOCATION-AT-PLACE
   	    :parameters (?agent ?place)
	    :precondition (and (agent ?agent)
	    		       (place ?place)
			       (at ?agent ?place))
	    :effect (and (lexicalized-agent-location ?agent ?place)))

   (:action LEXICALIZE-AGENT-LOCATION-AT-BUILDING
   	    :parameters (?agent ?building)
	    :precondition (and (agent ?agent)
	    		       (building ?building)
			       (at ?agent ?building))
	    :effect (and (lexicalized-agent-location ?agent ?building)))

   (:action LEXICALIZE-AGENT-TRAVEL-COMMAND-FROM-PLACE-TO-BUILDING
   	    :parameters (?agent ?action ?from ?to ?dir)
	    :precondition (and (agent ?agent)
	    		       (travel-action ?action)
			       (place ?from)
	    		       (building ?to)
			       (direction ?dir)
			       (at ?agent ?from)
			       (facing ?agent ?dir))
	    :effect (and (lexicalized-agent-travel-command ?agent ?action ?from ?to ?dir)
	    	    	 (at ?agent ?to)
			 (not (at ?agent ?from)))
	    )

   (:action LEXICALIZE-AGENT-TRAVEL-ADVICE-FROM-PLACE-TO-BUILDING
   	    :parameters (?agent ?action ?from ?to ?dir)
	    :precondition (and (agent ?agent)
	    		       (travel-action ?action)
			       (place ?from)
	    		       (building ?to)
			       (direction ?dir)
			       (at ?agent ?from)
			       (facing ?agent ?dir))
	    :effect (and (lexicalized-agent-travel-advice ?agent ?action ?from ?to ?dir)
	    	         (at ?agent ?to)
			 (not (at ?agent ?from))))

   (:action LEXICALIZE-AGENT-TRAVEL-COMMAND-FROM-BUILDING-TO-PLACE
   	    :parameters (?agent ?action ?from ?to ?dir)
	    :precondition (and (agent ?agent)
	    		       (travel-action ?action)
			       (building ?from)
	    		       (place ?to)
			       (direction ?dir)
			       (at ?agent ?from)
			       (facing ?agent ?dir))
	    :effect (and (lexicalized-agent-travel-command ?agent ?action ?from ?to ?dir)
	    	    	 (at ?agent ?to)
			 (not (at ?agent ?from))))

   (:action LEXICALIZE-AGENT-TRAVEL-ADVICE-FROM-BUILDING-TO-PLACE
   	    :parameters (?agent ?action ?from ?to ?dir)
	    :precondition (and (agent ?agent)
	    		       (travel-action ?action)
			       (building ?from)
	    		       (place ?to)
			       (direction ?dir)
			       (at ?agent ?from)
			       (facing ?agent ?dir))
	    :effect (and (lexicalized-agent-travel-advice ?agent ?action ?from ?to ?dir)
	    	    	 (at ?agent ?to)
			 (not (at ?agent ?from))))

   (:action LEXICALIZE-AGENT-TURN-COMMAND-AT-PLACE
   	    :parameters (?agent ?action ?place ?old-dir ?new-dir)
	    :precondition (and (agent ?agent)
	    		       (turn-action ?action)
			       (place ?place)
	    		       (direction ?old-dir)
			       (direction ?new-dir)
			       (facing ?agent ?old-dir))
	    :effect (and (lexicalized-agent-turn-command ?agent ?action ?place ?old-dir ?new-dir)
	    	    	 (facing ?agent ?new-dir)
			 (not (facing ?agent ?old-dir))))

   (:action LEXICALIZE-AGENT-TURN-ADVICE-AT-PLACE
   	    :parameters (?agent ?action ?place ?old-dir ?new-dir)
	    :precondition (and (agent ?agent)
	    		       (turn-action ?action)
			       (place ?place)
	    		       (direction ?old-dir)
			       (direction ?new-dir)	
			       (facing ?agent ?old-dir))
	    :effect (and (lexicalized-agent-turn-advice ?agent ?action ?place ?old-dir ?new-dir)
	    	    	 (facing ?agent ?new-dir)
			 (not (facing ?agent ?old-dir))))

   (:action LEXICALIZE-AGENT-DIRECTION
   	    :parameters (?agent ?dir)
	    :precondition (and (agent ?agent)
	    		       (direction ?dir)
			       (facing ?agent ?dir))
	    :effect (and (lexicalized-agent-direction ?agent ?dir)))

)
			       
