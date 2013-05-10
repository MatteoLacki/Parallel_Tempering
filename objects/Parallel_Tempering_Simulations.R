setClass(
	Class		= "Parallel_Tempering_Simulations",
	representation	= representation(
				No_of_Chains	= "numeric",					
					# Generated rather than provided.
				Initial_Points 	= "numeric",
				From_Lexic_Matrix = "matrix"
			),

	prototype	= prototype(
				No_of_Chains	= numeric(0),
					# Generated rather than provided.
				Initial_Points 	= numeric(0),
				From_Lexic_Matrix = matrix(nrow=0, ncol=0)				
			),
	contains	= "Simulations"
)
	
	# Overrided methods

setMethod(
	f 		= "Make_a_Step_of_the_Algorithm",
	signature 	= "Parallel_Tempering_Simulations",

	definition	= function( simulation, ... ) { 
				print( "I am the step of Parallel Tempering algorithm." )	
			}	
)

	# Class specific methods

setGeneric(	"simulate",
		def = function( simulation, ... ){ 	standardGeneric("simulate") }
)

