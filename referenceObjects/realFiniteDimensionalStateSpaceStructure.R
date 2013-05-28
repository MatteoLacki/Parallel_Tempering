realFiniteDimensionalStateSpaceStructure <- setRefClass(
	Class		= "realFiniteDimensionalStateSpaceStructure",
	contains	= "stateSpaceStructure",

###########################################################################
								# Fields

	fields		= list(

		problemDimension	= "integer",	#stored in two different entities
		noOfChains			= "integer",	#stored in two different entities
		noOfSteps 			= "integer",

		targetDensity 		= "function",

		simultatedStates						= "matrix",
		currentStates 							= "matrix",
		proposalsCovarianceCholeskised 			= "matrix",
		quasiMetric  							= "function"
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation


		initializeRealFiniteDimensionalStateSpaceStructure = function(
			noOfSteps 			= 0L,
			noOfChains 			= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			initialStates 		= matrix(ncol=0, nrow=0)
		)
		{
			tmpProblemDimension	<- as.integer(problemDimension)
			if ( is.na(tmpNoOfSteps) || (noOfSteps < 0) ) 
			{
				stop("Inappropriate problem dimension. Please enter an integer value.")
			} else
			{	
				problemDimension 		<<- tmpProblemDimension
			}

				# Checked already by the Simulation.
			noOfSteps 			<<- noOfSteps
			noOfChains			<<- noOfChains

				# Trusted to be ok.
			targetDensity 		<<- targetDensity	
			simultatedStates 	<<- 
				matrix(
					nrow = problemDimension*(2*noOfSteps + 1), 
					ncol = noOfChains
				)


		},

		initialize	= function(
			noOfSteps 			= 0L,  
			noOfChains 			= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			initialStates 		= matrix(ncol=0, nrow=0)
		)
		{
			initializeStateSpaceStructure(
				noOfSteps 			= noOfSteps
			)

			initializeRealFiniteDimensionalStateSpaceStructure(
				noOfChains 		= noOfChains,
				problemDimension= problemDimension,
				targetDensity 	= targetDensity,
				initialStates 	= initialStates 
			)
		},

		############################################################
				# Visualisation




		############################################################
				# Data structure methods.


			# Getting to right place in the matrix with states. 
		numeration	= function(
			noOfSavedIteration 	= 0, 
			type 				= 'init'
		)
		{
			init <- if (type =='init') 1 else 
					if (type =='RW')   (2*noOfSavedIteration - 1)*problemDimension+1 else
					if (type =='Swap') 2*noOfSavedIteration*problemDimension +1

			return( init:(init+problemDimension-1))
		},





		############################################################
				# Algorithmic Methods


		updateLogsOfUnnormalisedDensities = function(
			indicesOfStatesToUpdate
		)
		{
			return(
				log(
					apply( 
						Current_States[,indicesOfStatesToUpdate], 
						2, 
						targetDensity 
					)
				)	
			)
		}

###########################################################################
				# Finis Structurae
	)
)