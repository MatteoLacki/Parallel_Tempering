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
			iterationsNo 	= NULL
		){	
			iterationsNo <<- as.integer(iterationsNo)
		},

		initialize 	= function(
			iterationsNo 	= NULL
		){
			if ( !is.null(iterationsNo)){
				initializeStateSpace(
					iterationsNo = iterationsNo
				)
			}	
		},

		############################################################
				# Visualisation

		showState	= function()
		{},		

		prepareDataForPlot = function()
		{},

		showStateSpace 	= function()
		{
			cat('\nThe general state-space inputs are here: \n')
			cat('Number of iterations of the algorithm: ', iterationsNo, '\n\n')
		},

		show = function()
		{
			showStateSpace()
		},

		############################################################
				# Algorithmic Methods				

		proposeLogsOfUMeasures 		= function()
		{
			return( NULL )
		},

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