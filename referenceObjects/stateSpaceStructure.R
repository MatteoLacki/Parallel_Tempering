stateSpaceStructure <- setRefClass(
	Class		= "stateSpaceStructure",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the parallel tempering algorithm.	
		noOfIterations 		= "integer"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initializeStateSpaceStructure = function(
			noOfIterations 	= 0L
		)
		{
			tmpNoOfIterations <- as.integer( noOfIterations )
			if ( is.na(tmpNoOfIterations) || (tmpNoOfIterations < 0) ) 
			{
				stop("Inappropriate no of steps. Please enter an integer value.")
			} else
			{	
				noOfIterations 		<<- tmpNoOfIterations
			}
		},

		initialize 	= function(
			noOfIterations 	= 0L
		)
		{
			initializeStateSpaceStructure(
				noOfIterations 			= noOfIterations
			)
		}

		############################################################
				# Visualisation


####################################################################
				# Finis Structurae		
	)
)

source("./referenceObjects/realFiniteDimensionalStateSpaceStructure.R")