source("./referenceObjects/stateSpaceStructure.R")

Simulation <- setRefClass(
	Class		= "Simulations",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the algorithm.
		noOfIterations  	= "integer",

			## The data container with methods that act on it.
		stateSpaceStructure	= "stateSpaceStructure"	
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		simulationInitializator	= function(
			noOfIterations 	= 0L
			)
		{
			tmpNoOfIterations 	<- as.integer(noOfIterations)
			
			if ( is.na(tmpNoOfIterations) || (noOfIterations < 0) ) 
			{
				stop("Inappropriate number of steps. Please enter an integer value.")
			} else
			{	
				noOfIterations 	<<- tmpNoOfIterations
			}
		},	

		initialize = function(
			noOfIterations 	= 0L
			)
		{
			simulationInitializator(
				noOfIterations 	= noOfIterations			
			)
		},

		############################################################
				# Visualisation


		simulationShow = function()
		{
			cat('\n Welcome to our simulation! \n')
			cat('Number of steps: ', noOfIterations, '\n')
		},	

		show = function()
		{
			simulationShow()
		},

		############################################################
				# Algorithmic Methods


		makeStepOfTheAlgorithm	= function( 
			iteration 
		)
		{
			cat('I shall make it all happen.')
		},

		simulate = function()
		{
			# for ( iteration in 1:noOfIterations ) makeStepOfTheAlgorithm( iteration ) 
			tmp <- sapply( 
				1:noOfIterations, 
				function( iteration ) 
				{
					makeStepOfTheAlgorithm( iteration )
				},
				USE.NAMES = FALSE
			)

			rm(tmp)
		}	

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that!
Simulation$lock( names( Simulation$fields() ) )