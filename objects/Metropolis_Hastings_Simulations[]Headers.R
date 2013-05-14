setClass(
	Class		= "Metropolis_Hastings_Simulations",
	representation	= representation(),

	prototype	= prototype(),

	contains	= "Simulations"
)

	# Overrided methods

setMethod(
	f 			= "Make_a_Step_of_the_Algorithm",
	signature 	= "Metropolis_Hastings_Simulations",

	definition	= function( simulation, ... ){ 
				print( "I am the step of Metropolis-Hastings algorithm." )	
			}	
)

	# Class specific methods

# To do : many things.