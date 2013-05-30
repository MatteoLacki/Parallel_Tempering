
realFiniteDimensionalStateSpaceStructure <- setRefClass(
	Class		= "realFiniteDimensionalStateSpaceStructure",
	contains	= "stateSpaceStructure",

###########################################################################
								# Fields

	fields		= list(

			## Dimension of the original state space of interest.
		problemDimension	= "integer",	

			## Number of temperature levels.
		noOfTemperatures	= "integer",

			## The targetted density function, preserved by the algorithm's kernel.
		targetDensity 		= "function",

			## Matrix containing all simulated points.
		simulatedStates		= "matrix",

			## Matrix containing points simulated in the last step of the algorithm.
		currentStates 		= "matrix",

			## Quasi metric between two points from the state space.
		quasiMetric  		= "function",

			## The number of the last cell complex in the data metrix where something was inserted. If zero then nothing was inserted.
		freeSlotNo 			= "integer",

			## Number of cell complexes in the data structure
		noOfSlots 			= "integer"

			## Matrix containing covariances for the proposal kernel.
		#proposalsCovarianceCholeskised 	= "matrix"

	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation


		initializeRealFiniteDimensionalStateSpaceStructure = function(
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){}
		)
		{
				# Checked already by the Simulation.
			#noOfTemperatures		<<- noOfTemperatures

			initialStatesDimension 	<- nrow(initialStates)
			initialStatesnoOfTemperatures	<- ncol(initialStates)
			tmpProblemDimension		<- as.integer(problemDimension)
			tmpNoOfTemperatures		<- noOfTemperatures


			ifelse( 
				(tmpProblemDimension==0L | tmpNoOfTemperatures ==0L), 
				ifelse( 
					(initialStatesDimension > 0L & initialStatesnoOfTemperatures > 0L), 
					{
						currentStates	<<- initialStates

							# Consider input data representative.
						problemDimension<<- initialStatesDimension
						noOfTemperatures 		<<- initialStatesnoOfTemperatures

						cat('\nWe infered from the initial states that\n a) problem has dimension equal to ',problemDimension,'\n b) there are ',noOfTemperatures,' chains to be consdered.\n')
					},	
					stop("\nYou did not provide enough information to autogenerate initial states or did not provide the initial states yourself. Good job! You're really getting easily into troubles now...\n")
				),
				{
					problemDimension<<- tmpProblemDimension
					noOfTemperatures<<- tmpNoOfTemperatures

					ifelse(
						( 	
							tmpProblemDimension == initialStatesDimension & 
							tmpNoOfTemperatures 		== initialStatesnoOfTemperatures
						),
						{	
							currentStates	<<- initialStates
						},
						ifelse(
							( 
								initialStatesDimension ==0 |
								initialStatesnoOfTemperatures==0
							),
							{		# No initial states supplied. Enough info to generate them.
								currentStates <<- 
									replicate( 
										n 	= noOfTemperatures, 
										expr= runif( 
											n = problemDimension,
											min=0,
											max=10 
										)
									)	
							},
								# Inconsistency...
							stop("\nThe initial states you supplied differ in dimension or differ in number of chains.\n")
						)
					)
				}
			)	

			simulatedStates	<<- 
				matrix(
					nrow = problemDimension*(2*noOfIterations + 1), 
					ncol = noOfTemperatures
				)

			freeSlotNo 		<<- 1L
			noOfSlots 		<<- noOfIterations*2L + 1L

			insertStates( currentStates )

			rm( tmpProblemDimension, tmpNoOfTemperatures )

			targetDensity	<<- targetDensity	
			quasiMetric 	<<- quasiMetric
		},

		initialize	= function(
			noOfIterations 		= 0L,  
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){}
		)
		{
			initializeStateSpaceStructure(
				noOfIterations 		= noOfIterations
			)

			initializeRealFiniteDimensionalStateSpaceStructure(
				noOfTemperatures = noOfTemperatures,
				problemDimension = problemDimension,
				targetDensity 	 = targetDensity,
				initialStates 	 = initialStates,
				quasiMetric 	 = quasiMetric 
			)
		},

		############################################################
				# Visualisation




		############################################################
				# Data structure methods.


		simulationTerminated = function()
		{
			return( freeSlotNo == noOfSlots + 1L )
		},

		insertStates	= function(
			inputMatrix
		)
		{
			ifelse( 
				freeSlotNo <= noOfSlots,
				{
					simulatedStates[
						((freeSlotNo-1)*problemDimension+1):
						(freeSlotNo*problemDimension),
					] 	<<- inputMatrix

					freeSlotNo 	<<- freeSlotNo + 1L
				},
				stop('The computer tried to make more steps than the user wanted him to. That is truly weird...') 
			)
		},

		getStates 	= function(
			whichSlotNo
		)
		{
				# It's ok to return this: this is not a pointer, but a copied submatrix
			return(
				simulatedStates[ 
					((whichSlotNo-1)*problemDimension+1):
					(whichSlotNo*problemDimension),
				]
			)
		},

			# Type: either 'initial states', 'random walk', 'swap'. 
		getIteration	= function(
			iteration 	= 0L,
			type		= 'initial states' 
		)
		{
			ifelse( 
				simulationTerminated(),
				{
					result 	<- getStates(
						ifelse(
							type = 'initial states',
							1L,
							ifelse( 
								type = 'random walk',
								2L*iteration,
								ifelse(
									type = 'swap',
									2L*iteration+1L,
									stop("Error: you can choose only among types such as 'initial states', 'random walks', or 'swap'. " )
								)
							)
						)	
					)			
				},
				{
					cat('Simulation not yet terminated. Here are the initial states.\n')
					result 	<- getStates(1L) 
				}
			)
			return( result )
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