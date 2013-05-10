library(methods)

#source("./objects/Simulation_Inputs.R")

setClass(
	Class		= "Simulations",
	representation	= representation(
				No_of_Steps 	= "numeric",
				Names_of_Slots_for_Table_in_Show_Method = "character",
				"VIRTUAL"	
			),

	prototype	= prototype(
				No_of_Steps 	= numeric(0),
				Names_of_Slots_for_Table_in_Show_Method = c("Number of steps")
			)
)

	# Methods shared by all simulations / headers. (Fucking R so not C++)	

setGeneric(	"simulate",
		def = function( simulation, ... ){ 	standardGeneric("simulate") }
)
setGeneric( 	"Make_a_Step_of_the_Algorithm",
		def = function( simulation, ... ) { 	standardGeneric("Make_a_Step_of_the_Algorithm") }
)
setGeneric( 	"Make_a_Step_of_the_Algorithm",
		def = function( simulation, ... ) { 	standardGeneric("Make_a_Step_of_the_Algorithm") }
)
setGeneric( 	"Check_if_Initial_Data_was_provided",
		def = function( simulation, ... ) { 	standardGeneric("Check_if_Initial_Data_was_provided") }
)



	# Bodies of headers.

setMethod(
	f 		= "Make_a_Step_of_the_Algorithm",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am the step of abstract algorithm." )	
	}	
)

setMethod(
	f 		= "Check_if_Initial_Data_was_provided",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am checking if initial data was provided." )	
	}	
)

setMethod(
	f 		= "summary",
	signature 	= "Simulations",
	definition	= 
	function( object, ... ) { # The really stupid thing - names of args as in non-overwritten versions.
		print( "I summarise the simulation outcome." )	
	}	
)


setMethod(
	f 		= "simulate",
	signature 	= "Simulations",

	definition	= 
	function(simulation, ...){ 

		Check_if_Initial_Data_was_provided()		

		for( i in 1:simulation@No_of_Steps ) Make_a_Step_of_the_Algorithm()

		summary( simulation )  	# Overwrite standard summary.
	}	
)
