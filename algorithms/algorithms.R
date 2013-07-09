algorithm <- setRefClass(
	Class		= "Algorithms",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(
#<fields>
			## Number of iterations of the algorithm.
		iterationsNo  = "integer",

			## The data container with methods that act on it.
		stateSpace		= "StateSpaces",

			## Boolean value: TRUE if the simulation has been carried out.
		simulationFinished		= "logical"	
#</fields>		
	),

###########################################################################
								# Methods

	methods 	= list(	
		
		############################################################
				# Initialisation
#<method>
		initializeAlgorithm	= function(
			iterationsNo 	= 0L
			)
		{
			simulationFinished	<<- FALSE
			tmpIterationsNo 	<- as.integer(iterationsNo)
			
			if ( is.na(tmpIterationsNo) || (iterationsNo < 0) ) 
			{
				stop("Inappropriate number of steps. Please enter an integer value.")
			} else
			{	
				iterationsNo 	<<- tmpIterationsNo
			}
		},	

#<method>
		initialize = function(
			iterationsNo 	= 0L
			)
		{
			initializeAlgorithm(
				iterationsNo 	= iterationsNo			
			)
		},

#<method>
		prepareSimulation = function()
		{},

		############################################################
				# Visualisation
#<method>
		showAlgorithm = function()
		{
			cat('\n Welcome to our algorithm! \n')
			cat('Number of steps: ', iterationsNo, '\n')
		},	

#<method>
		show = function()
		{
			showAlgorithm()
		},

#<method>
		getDataForVisualisation = function()
		{
			stateSpace$prepareDataForPlot()
		},

		############################################################
				# Algorithmic Methods

#<method>
		makeStepOfTheAlgorithm	= function( 
			iteration 
		)
		{
			cat('I shall make it all happen.')
		},

#<method>
		simulate = function()
		{
			prepareSimulation()

			tmp <- sapply( 
				1:iterationsNo, 
				function( iteration ) 
				{
					makeStepOfTheAlgorithm( iteration )
				},
				USE.NAMES = FALSE
			)

			rm(tmp)

			getDataForVisualisation()

			simulationFinished	<<- TRUE
		}	

###########################################################################
				# Finis Structurae	
	)
)

	# This will lock all fields. We want that!
#algorithm$lock( names( algorithm$fields() ) )