stateSpace <- setRefClass(
	Class		= "StateSpaces",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the parallel tempering algorithm.	
		iterationsNo 		= "integer"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initializeStateSpace = function(
			iterationsNo 	= 0L
		)
		{
			tmpIterationsNo <- as.integer( iterationsNo )
			if ( is.na(tmpIterationsNo) || (tmpIterationsNo < 0) ) 
			{
				stop("Inappropriate no of steps. Please enter an integer value.")
			} else
			{	
				iterationsNo 		<<- tmpIterationsNo
			}
		},

		initialize 	= function(
			iterationsNo 	= 0L
		)
		{
			initializeStateSpace(
				iterationsNo 			= iterationsNo
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

		getProposalLogsOfUDensities = function()
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