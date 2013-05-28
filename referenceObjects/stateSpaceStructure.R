stateSpaceStructure <- setRefClass(
	Class		= "stateSpaceStructure",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(
		noOfChains			= "integer",		
		currentIteration	= "integer"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initializeStateSpaceStructure = function(
			noOfSteps 			= 0L,
		)
		{
			tmpNoOfSteps <- as.integer( noOfSteps )
			if ( is.na(tmpNoOfSteps) || (tmpNoOfSteps < 0) ) 
			{
				stop("Inappropriate no of steps. Please enter an integer value.")
			} else
			{	
				noOfSteps 		<<- tmpNoOfSteps
			}

			currentIteration 	<<- 0
		}

		initialize 	= function(
			noOfSteps 			= 0L
		)
		{
			initializeStateSpaceStructure(
				noOfSteps 			= noOfSteps
			)
		}

		############################################################
				# Visualisation


		############################################################
				# Finis Structurae		
	)
)