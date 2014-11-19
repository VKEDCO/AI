;;; USU Main Quad
;;; author: Vladimir Kulyukin
(define (domain usu-main-quad)
   (:requirements :strips :equality)
   (:predicates
	(building ?b)
	(direction ?d)
	(path ?from ?to ?dir)
	(facing ?agent ?dir)
	(agent ?a)
	(at ?agent ?place)
	(place ?p))

(:action TRAVEL01
	 :parameters (?agent ?from ?to ?dir)
	 :precondition (and (building ?from)
	 	       	    (place ?to)
			    (direction ?dir)
			    (at ?agent ?from)
			    (facing ?agent ?dir)
			    (path ?from ?to ?dir))
         :effect (and (at ?agent ?to) (not (at ?agent ?from))))

(:action TRAVEL02
	 :parameters (?agent ?from ?to ?dir)
	 :precondition (and (place ?from)
	 	       	    (building ?to)
			    (direction ?dir)
			    (at ?agent ?from)
			    (facing ?agent ?dir)
			    (path ?from ?to ?dir))
         :effect (and (at ?agent ?to) (not (at ?agent ?from))))

(:action TURN
	 :parameters (?agent ?place ?olddir ?newdir)
	 :precondition (and (agent ?agent)
	 	       	    (at ?agent ?place)
			    (facing ?agent ?olddir)
	 	       	    (place ?place)
			    (direction ?olddir)
			    (direction ?newdir))
         :effect (and (facing ?agent ?newdir)
	 	      (not (facing ?agent ?olddir))))
	 	      
)
