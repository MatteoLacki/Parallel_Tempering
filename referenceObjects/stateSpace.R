stateSpace <- setRefClass(
	Class		= "StateSpaces",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the parallel tempering algorithm.	
		iterationsNo	= "integer",

			## The sub-object storing information needed for evaluation of the unnormalised density.
		targetMeasure	= "TargetMeasures"	
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
			print("\nHERE3\n")	

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

		proposeLogsOfUMeasures 		= function()
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