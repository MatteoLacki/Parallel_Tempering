library(methods)

setClass(
	Class		= "Simulations",
	representation	= representation(
					# User provided
				No_of_Steps 		= "integer",
				Problem_Dimension	= "integer",
				Target_Density 		= "function",
				Initial_Points		= "matrix",

					# Automatically initialized.

				Simulated_Chains	= "list",
				Current_States 		= "numeric",
				
					# Needed in iteration of the algorithm
				Log_Densities_of_Current_States	= "matrix",

					# Class names
				Slots_Names_Simulations			= "character",
				Slots_Integer_Simulations		= "character",
				Slots_Numeric_Simulations		= "character",

				"VIRTUAL"	
			),

	prototype	= prototype(
				No_of_Steps 		= integer(0),
				Simulated_Chains 	= list(),
				Target_Density 		= function(){print("Hello. You should add a function.")},
				Problem_Dimension 	= integer(0),
				Initial_Points		= matrix(nrow=0, ncol=0),
				Current_States 		= numeric(0),

			 	Log_Densities_of_Current_States	= matrix(nrow=0, ncol=0),

				Slots_Integer_Simulations		= c("No_of_Steps", "Problem_Dimension"),
				Slots_Numeric_Simulations		= c("Initial_Points", "Current_States")

			),

	validity 	= function( object )
		{
			# Optimal code: would check and write what is still not there.	

			cat("Inspector validates the data. \n")
			#if ( FALSE )
			#{
    		#	stop("[Simulations: validation] you ommited some of necessary input.")
			#} else{}
			
			return(TRUE)	
		} 


)

	#  headers of methods for Simulations

setGeneric(		"simulate",
		def = function( simulation, ... ){ 	standardGeneric("simulate") }
)
setGeneric( 	"Make_a_Step_of_the_Algorithm",
		def = function( simulation, ... ){ 	standardGeneric("Make_a_Step_of_the_Algorithm") }
)
setGeneric( 	"Check_if_Initial_Data_was_provided",
		def = function( simulation, ... ){ 	standardGeneric("Check_if_Initial_Data_was_provided") }
)
setGeneric(		"Call_Target_Density",
		def = function( simulation, ... ){ 	standardGeneric("Call_Target_Density") }
)
setGeneric(		"Update_Logs_of_Unnormalised_Densities_in_Current_States",
		def = function( simulation, ... ){ 	standardGeneric("Update_Logs_of_Unnormalised_Densities_in_Current_States") }
)



source("./objects/Simulations[]Methods.R")