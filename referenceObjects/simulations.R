#source("./objects/.R")

Simulation <- setRefClass(
	Class		= "Simulations",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

		noOfSteps   		= "integer",
		stateSpaceSphere	= "stateSpaceStructure"	
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		simulationInitializator	= function(
			noOfSteps 			= 0L
			...
			)
		{
			tmpNoOfSteps 	<- as.integer(noOfSteps)
			
			if ( is.na(tmpNoOfSteps) || (noOfSteps < 0) ) 
			{
				stop("Inappropriate number of steps. Please enter an integer value.")
			} else
			{	
				noOfSteps 		<<- tmpNoOfSteps
			}
		},	

		initialize 				= function(
			noOfSteps 			= 0L
			...
			)
		{
			simulationInitializator(
				noOfSteps 		=noOfSteps, 
				...
			)
		},

		############################################################
				# Visualisation


		simulationShow = function()
		{
			cat('\n Welcome to our simulation! \n')
			cat('Number of steps: ', noOfSteps, '\n')
		},	

		show	= function()
		{
			simulationShow()
		}

		############################################################
				# Algorithmic Methods


		makeStepOfTheAlgorithm	= function()
		{
			cat('I shall make it all happen.')
		}

		simulate = function()
		{
			for ( iteration in 1:noOfSteps ) makeStepOfTheAlgorithm() 
		}	

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that!
Simulation$lock( names( Simulation$fields() ) )