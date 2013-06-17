algorithm <- setRefClass(
	Class		= "Algorithms",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Number of iterations of the algorithm.
		noOfIterations  = "integer",

			## The data container with methods that act on it.
		stateSpace		= "StateSpaces"	
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation

		initializeAlgorithm	= function(
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
			initializeAlgorithm(
				noOfIterations 	= noOfIterations			
			)
		},

		prepareSimulation = function()
		{},

		############################################################
				# Visualisation

		algorithmShow = function()
		{
			cat('\n Welcome to our algorithm! \n')
			cat('Number of steps: ', noOfIterations, '\n')
		},	

		show = function()
		{
			algorithmShow()
		},

		getDataForVisualisation = function()
		{
			stateSpace$prepareDataForPlot()
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
			prepareSimulation()

			tmp <- sapply( 
				1:noOfIterations, 
				function( iteration ) 
				{
					makeStepOfTheAlgorithm( iteration )
				},
				USE.NAMES = FALSE
			)

			rm(tmp)

			getDataForVisualisation()
		}	

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that!
algorithm$lock( names( algorithm$fields() ) )