;;; Find a path from Ray B West to Agricultural Sciences
;;; Author: Vladimir Kulyukin
(define (problem usu_quad_prob02)
	(:domain usu-main-quad)
  	(:objects OldMain AnimalSciences RayBWest AgriculturalSciences MiddleOfQuad East North South West Agent01)
   	(:init 
	   (building OldMain) 
	   (building AnimalSciences) 
	   (building RayBWest) 
	   (building AgriculturalSciences)
	   (place MiddleOfQuad)
	   (direction East) 
	   (direction North) 
	   (direction South) 
	   (direction West)
	   (agent Agent01)
	   (at Agent01 RayBWest)
	   (facing Agent01 North)
	   (path OldMain MiddleOfQuad East)
	   (path MiddleOfQuad OldMain West)
	   (path MiddleOfQuad AnimalSciences North)
	   (path AnimalSciences MiddleOfQuad South)
	   (path RayBWest MiddleOfQuad North)
	   (path MiddleOfQuad RayBWest South)
	   (path MiddleOfQuad AnimalSciences East)
	   (path AnimalSciences MiddleOfQuad West)
	   (path MiddleOfQuad AgriculturalSciences East)
	   (path AgriculturalSciences MiddleOfQuad West)
	   )
	(:goal (and (at Agent01 AgriculturalSciences)
	            (facing Agent01 East)
		    )))
