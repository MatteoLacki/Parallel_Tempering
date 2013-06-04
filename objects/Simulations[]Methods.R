	# Initalizators

setMethod(
	f 		= "initialize",
	signature 	= "Simulations",
	definition	= 

	function( 	.Object, 
				No_of_Steps,
				Problem_Dimension,
				Target_Density,
				Initial_Points,
				... 
			) 
	{ 
		cat("Initializator builds up the Simulation after the 'new'. \n")	

		.Object@Slots_Names_Simulations <- 
			c(	.Object@Slots_Integer_Simulations,
				.Object@Slots_Numeric_Simulations)

		.Object@No_of_Steps 		<- as.integer(No_of_Steps)
		.Object@Problem_Dimension 	<- as.integer(Problem_Dimension)
		.Object@Target_Density 		<- Target_Density
		
		.Object@Initial_Points 		<- Initial_Points

		validObject(.Object)

		return(.Object)
	}	
)




	# For the simulation purposes.

setMethod(
	f 		= "Make_a_Step_of_the_Algorithm",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am the step of abstract algorithm." )	
	}	
)

setMethod(
	f 		= "Call_Target_Density",
	signature 	= "Simulations",
	definition	= 

	function( simulation, ... ) { 
		print( "I am calling the target density." )	
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
	signature 	= "Simulations",

	definition	= function( simulation, ... ) 
		{ 
		return(	
				log(
					apply( 
						simulation@Current_States, 
						2, 
						simulation@Target_Density 
						# simulation@Call_Target_Density
					)
				)
			)				
		}	
)

setMethod(
	f 		= "simulate",
	signature 	= "Simulations",

	definition	= 
	
	function(simulation, ...){ 

		Check_if_Initial_Data_was_provided(simulation)		

		for( i in 1:simulation@No_of_Steps ) { Make_a_Step_of_the_Algorithm(simulation) }

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


