algorithm <- setRefClass(
	Class		= "Algorithms",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the algorithm.
		iterationsNo  = "integer",

			## The data container with methods that act on it.
		stateSpace		= "StateSpaces",

			## Boolean value: TRUE if the simulation has been carried out.
		simulationFinished		= "logical",

			## Burn-in period.
		burnIn				= "integer"	
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		initialize = function(
			iterationsNo 	= NULL,
			burnIn 			= 2000L,
			...
		){
			if ( !is.null(iterationsNo)){
				burnIn 				<<- burnIn
				simulationFinished	<<- FALSE
				tmpIterationsNo 	<- as.integer(iterationsNo)
			
				if ( is.na(tmpIterationsNo) || (iterationsNo < 0) ){
					stop("Inappropriate number of steps. Please enter an integer value.")
				} else {	
					iterationsNo 	<<- tmpIterationsNo
				}	
			}
		},
			

		prepareSimulation = function()
		{},

		############################################################
				# Visualisation

		show = function(...)
		{
			anteSimulationShow()
			postSimulationShow()
		},


		anteSimulationShow = function(...)
		{
			cat('\n Welcome to our algorithm! \n')
			cat('Number of steps: ', iterationsNo, '\n')		
		},


		postSimulationShow = function(...)
		{
			cat('Do not forget to define 
				what I show after the simulation
				has finished. ')		
		},


		getDataForVisualisation = function()
		{
			stateSpace$prepareDataForPlot()
		},

		############################################################
				# Algorithmic Methods


		makeStepOfTheAlgorithm	= function( 
			iteration,
			burning = FALSE 
		)
		{
			cat('I shall make it all happen.')
		},


		simulate = function()
		{
			prepareSimulation()
			
			iteration <- 1L
			while( iteration <= burnIn ){
				makeStepOfTheAlgorithm( iteration )
				iteration <- iteration+1	
			}

			stateSpace$turnOffBurnIn()

			iteration <- 1L
			while( iteration <= iterationsNo ){
				makeStepOfTheAlgorithm( iteration )
				iteration <- iteration+1	
			}

			# tmp <- sapply( 
			# 	1:iterationsNo, 
			# 	function( iteration ) 
			# 	{
			# 		makeStepOfTheAlgorithm( iteration )
			# 	},
			# 	USE.NAMES = FALSE
			# )

			# rm(tmp)

			getDataForVisualisation()

			simulationFinished	<<- TRUE
		}	

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that!
#algorithm$lock( names( algorithm$fields() ) )