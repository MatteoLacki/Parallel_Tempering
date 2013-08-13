stateSpace <- setRefClass(
	Class		= "StateSpaces",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the parallel tempering algorithm.	
		iterationsNo	= "integer",

			## Burn-in period.
		notBurning			= "boolean",

			## The sub-object storing information needed for evaluation of the unnormalised density.
		targetMeasure	= "TargetMeasures",

		spaceName 		= "character"	
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function(
			iterationsNo 	= NULL,
			...
		){
			if ( !is.null(iterationsNo)){			
				iterationsNo 	<<- as.integer(iterationsNo)
				spaceName 	 	<<- 'General State Space'
				notBurning 		<<- FALSE
			}	
		},


		turnOffBurnIn = function(){
			notBurning <<- TRUE
		}

		############################################################
				# Visualisation

		showState	= function()
		{},		


		prepareDataForPlot = function()
		{},


		show = function(...)
		{
			anteSimulationShow()
			postSimulationShow()	
		},


		anteSimulationShow = function(...)
		{
			cat('\nUsing General State Space.\n')
			cat('\tNumber of iterations of the algorithm: ', iterationsNo, '\n\n')
		},


		postSimulationShow = function(...)
		{
			cat('Do not forget to define 
				what I show after the simulation
				has finished. ')		
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