library(methods)

source("./objects/Simulation_Inputs.R")

setClass(
	Class		= "Simulations",
	representation	= representation(
				No_of_Steps = "numeric",
				Input 		= "Simulation_Inputs"	
			),

	prototype	= prototype(N
				No_of_Steps 	= numeric(0),
				
			)
	# Add validator.
)

setGeneric(
	"simulate",
	def 		= function( simulation_instatiation ) 
			{
				standardGeneric("simulate")	
			}
)

setMethod(
	f 		= "simulate",
	signature 	= "Simulations",

	definition	= function(object) { 
			
			object@initial_Step()

			for( i in 1:object@No_of_Steps )
	
				


			}
		}
	
)

