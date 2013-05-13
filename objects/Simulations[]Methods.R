setMethod(
	f 		= "Make_a_Step_of_the_Algorithm",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am the step of abstract algorithm." )	
	}	
)

setMethod(
	f 		= "Call_target_Density",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am calling the target density." )	

		# Should take as argument a function really and evaluate it.
		# Stupid - how to store it? Fucking stupid.
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
	f 		= "Update_Logs_of_Unnormalised_Densities_in_Current_States",

	signature 	= "Parallel_Tempering_Simulations",

	definition	= function( simulation, ... ) { 
				print( "I am the step of Parallel Tempering algorithm." )	
			}	
)

setMethod(
	f 		= "simulate",
	signature 	= "Simulations",

	definition	= 
	
	function(simulation, ...){ 

		Check_if_Initial_Data_was_provided()		

		for( i in 1:simulation@No_of_Steps ) { Make_a_Step_of_the_Algorithm() }

		summary( simulation )  	# Overwrite standard summary.
	}	
)

	# Just for show-off.

setMethod(
	f 		= "summary",
	signature 	= "Simulations",
	definition	= 

	function( object, ... ) { # The really stupid thing - names of args as in non-overwritten versions.
		print( "I summarise the simulation outcome." )	
	}	
)


