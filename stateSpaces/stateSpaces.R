stateSpace <- setRefClass(
	Class		= "StateSpaces",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the parallel tempering algorithm.	
		iterationsNo	= "integer",

			## Burn-in period.
		notBurning		= "logical",

			## The sub-object storing information needed for evaluation of the unnormalised density.
		targetMeasure	= "TargetMeasures",

		integrant 		= "FunctionsToIntegrate",

		spaceName 		= "character",

		rememberStates 	= "logical"	
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 			= function(
			iterationsNo 	= NULL,
			rememberStates  = FALSE,
			...
		){
			if ( !is.null(iterationsNo)){			
				iterationsNo 	<<- as.integer(iterationsNo)
				spaceName 	 	<<- 'General State Space'
				rememberStates  <<- rememberStates
				
					# Burn-in is always initially turned-off, so that the initial states get stored properly. It is then announced by the algorithm. Finally, it is lifted by the algorithm.
				notBurning 		<<- TRUE
			}
		},

		turnOnBurnIn 		= function(){
			notBurning <<- FALSE
		},


		turnOffBurnIn 		= function(){
			notBurning <<- TRUE
		},

		############################################################
				# Visualisation

		showState			= function()
		{},		


		prepareDataForPlot 	= function()
		{},


		show 				= function(...)
		{
			anteSimulationShow()
			postSimulationShow()	
		},


		anteSimulationShow 	= function(...)
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


		writeStates = function(...)
		{
			cat('Do not forget to define 
				what I save after the simulation
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
		{},

		calculateBetweenSteps  		= function(...)
		{
			cat('Do not forget to define 
				what I calculate between steps.')		
		}
####################################################################
				# Finis Structurae		
	)
)