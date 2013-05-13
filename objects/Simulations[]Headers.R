library(methods)

setClass(
	Class		= "Simulations",
	representation	= representation(
				No_of_Steps 		= "numeric",
				Simulated_Chains	= "list"
				"VIRTUAL"	
			),

	prototype	= prototype(
				No_of_Steps 		= numeric(0),
				Simulated_Chains 	= list() 					
			)
)

	#  headers of methods for Simulations

setGeneric(	"simulate",
		def = function( simulation, ... ){ 	standardGeneric("simulate") }
)
setGeneric( 	"Make_a_Step_of_the_Algorithm",
		def = function( simulation, ... ){ 	standardGeneric("Make_a_Step_of_the_Algorithm") }
)
setGeneric( 	"Make_a_Step_of_the_Algorithm",
		def = function( simulation, ... ){ 	standardGeneric("Make_a_Step_of_the_Algorithm") }
)
setGeneric( 	"Check_if_Initial_Data_was_provided",
		def = function( simulation, ... ){ 	standardGeneric("Check_if_Initial_Data_was_provided") }
)
setGeneric(	"Call_target_Density",
		def = function( simulation, ... ){ 	standardGeneric("Call_target_Density") }
)
setGeneric(	"Update_Logs_of_Unnormalised_Densities_in_Current_States",
		def = function( simulation, ... ){ 	standardGeneric("Update_Logs_of_Unnormalised_Densities_in_Current_States") }
)



source("./objects/Simulations_Methods.R")