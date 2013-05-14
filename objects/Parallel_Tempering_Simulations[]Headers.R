setClass(
	Class			= "Parallel_Tempering_Simulations",
	representation	= representation(
				
				No_of_Chains		= "integer",



					# Generated rather than provided.
				From_Lexic_Matrix 	= "matrix"			 		

			),

	prototype	= prototype(
				
				No_of_Chains		= integer(0),
				
					# Generated rather than provided.
				From_Lexic_Matrix 	= matrix(nrow=0, ncol=0)
								
			),
	contains	= "Simulations"
)
	

#  headers of methods for Simulations

	# None for now.

source("./objects/Parallel_Tempering_Simulations[]Methods.R")