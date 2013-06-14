stateSpace <- setRefClass(
	Class		= "stateSpace",
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

		initializeStateSpace = function(
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
			initializeStateSpace(
				noOfIterations 			= noOfIterations
			)
		},

		############################################################
				# Visualisation

		showState	= function()
		{},		

		prepareDataForPlot = function()
		{},

		############################################################
				# Algorithmic Methods				

		getProposalLogsOfUnnormalisedDensities = function()
		{},

		randomWalkProposal 			= function()
		{},

		updateStatesAfterRandomWalk = function()
		{},

		updateStatesAfterSwap 		= function()
		{}


####################################################################
				# Finis Structurae		
	)
)

source("./referenceObjects/realStateSpace.R")