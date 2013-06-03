
realFiniteDimensionalStateSpaceStructure <- setRefClass(
	Class		= "realFiniteDimensionalStateSpaceStructure",
	contains	= "stateSpaceStructure",

###########################################################################
								# Fields

	fields		= list(

			## Dimension of the original state space of interest.
		problemDimension	= "integer",	

			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## Number of temperature levels.
		noOfTemperatures	= "integer",

			## The targetted density function, preserved by the algorithm's kernel.
		targetDensity 		= "function",

			## Matrix containing all simulated points.
		simulatedStates		= "matrix",

			## Matrix containing proposals of the random walk.
		proposedStates 		= "matrix",

			## Matrix containing points simulated in the last step of the algorithm: after random walk phase it is composed of previous current states with updates being the accepted proposals.
		lastStates 		= "matrix",

			## Quasi metric between two points from the state space.
		quasiMetric  		= "function",

			## The number of the last cell complex in the data metrix where something was inserted. If zero then nothing was inserted.
		freeSlotNo 			= "integer",

			## Number of cell complexes in the data structure
		noOfSlots 			= "integer",

			## Matrix containing covariances for the proposal kernel.
		proposalCovariancesCholeskised 	= "matrix",

			## Boolean: says whether proposal covariances are the same on different temperature levels.
		simpleCovariance	= "logical"	
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation


		initializeRealFiniteDimensionalStateSpaceStructure = function(
			temperatures 		= numeric(0),
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
		{
				# Checked already by the Simulation.
			temperatures 	<<- temperatures

			initialStatesDimension 	<- nrow(initialStates)
			initialStatesnoOfTemperatures	<- ncol(initialStates)
			tmpProblemDimension		<- as.integer(problemDimension)
			tmpNoOfTemperatures		<- noOfTemperatures


			ifelse( 
				(tmpProblemDimension==0L | tmpNoOfTemperatures ==0L), 
				ifelse( 
					(initialStatesDimension > 0L & initialStatesnoOfTemperatures > 0L), 
					{
						lastStates	<<- initialStates

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
							tmpNoOfTemperatures == initialStatesnoOfTemperatures
						),
						{	
							lastStates	<<- initialStates
						},
						ifelse(
							( 
								initialStatesDimension ==0 |
								initialStatesnoOfTemperatures==0
							),
							{		# No initial states supplied. Enough info to generate them.
								lastStates <<- 
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
				# These are the same as current because we want updateLogsOfUnnormalisedDensities to update the correct inital log densities.
			proposedStates 	<<- lastStates

			simulatedStates	<<- 
				matrix(
					nrow = problemDimension*(2*noOfIterations + 1), 
					ncol = noOfTemperatures
				)

			freeSlotNo 		<<- 1L
			noOfSlots 		<<- noOfIterations*2L + 1L

			insertStates()

			rm( tmpProblemDimension, tmpNoOfTemperatures )

			targetDensity	<<- targetDensity	
			quasiMetric 	<<- quasiMetric

			ifelse(
				(	class(proposalCovariances) == 'matrix' ),
				ifelse(
					( 
						nrow(proposalCovariances)==problemDimension &
						ncol(proposalCovariances)==problemDimension
					),
					{
						proposalCovariancesCholeskised <<- 
							chol( proposalCovariances )			
						simpleCovariance 	<<- TRUE
					},
					{
						cat('You supplied a covariance matrix that does not conform to our state space dimension or did not care to supply it at all.\n Proceeding with identity covariances.\n')
						
						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=problemDimension)
							)
						simpleCovariance <<- TRUE
					}
				),
				ifelse(
					(
						class(proposalCovariances) == 'list' &
						length(proposalCovariances)==noOfTemperatures
					),
					ifelse(
						all(
							sapply(
								proposalCovariances,
								function(x) 
									ifelse( 
										(class(x) == 'matrix'), 
										nrow(x)==2 & ncol(x)==2, 
										FALSE
									) 
							)
						),	
						{
							proposalCovariancesCholeskised <<-
								do.call( 
									cbind, 
									lapply( 
										proposalCovariances, 
										chol 
									) 
								)
							simpleCovariance 	<<- FALSE
						},
						{
							cat('Your covariances are either not matrices or their size do not conform to problem dimension.\n Proceeding with identity covariances.\n')

							proposalCovariancesCholeskised <<- 
								diag(
									rep.int(1, times=problemDimension)
								)
							simpleCovariance <<- TRUE	
						}
					),
					{
						cat('\nProposal covariances are not enlisted or are enlisted but the number of covariance matrices is other than the number of temperatures.\n Proceeding with identity covariances.\n')

						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=problemDimension)
							)
						simpleCovariance <<- TRUE	
					}
				)
			)
		},

		initialize	= function(
			temperatures 		= numeric(0),
			noOfIterations 		= 0L,  
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
		{
			initializeStateSpaceStructure(
				noOfIterations 		= noOfIterations
			)

			initializeRealFiniteDimensionalStateSpaceStructure(
				noOfTemperatures = noOfTemperatures,
				temperatures 	 = temperatures,
				problemDimension = problemDimension,
				targetDensity 	 = targetDensity,
				initialStates 	 = initialStates,
				quasiMetric 	 = quasiMetric,
				proposalCovariances = proposalCovariances
			)
		},


		############################################################
				# Data structure methods.


		simulationTerminated = function()
			#### Checks whether simulation is terminated.
		{
			return( freeSlotNo == noOfSlots + 1L )
		},

		insertStates	= function() 
			#### Inserts current states to the data history (field: simulatedStates).
		{
			ifelse( 
				freeSlotNo <= noOfSlots,
				{
					simulatedStates[
						((freeSlotNo-1)*problemDimension+1):
						(freeSlotNo*problemDimension),
					] 	<<- lastStates

					freeSlotNo 	<<- freeSlotNo + 1L
				},
				stop('The computer tried to make more steps than the user wanted him to. That is truly weird...') 
			)
		},

		getStates 	= function(
			whichSlotNo
		)
			#### Extracts the given slot from data history (field: simulatedStates).
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
			#### Extracts for a given iteration results of the given step type.
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
					cat('Simulation not yet terminated - here are the initial states:\n\n')
					result 	<- getStates(1L) 
				}
			)
			return( result )
		},

		updatelastStates = function(
			indicesOfStatesUpdatedInRandomWalk = NA	
		)
		{
			if(!is.na(indicesOfStatesUpdatedInRandomWalk))
				lastStates[,indicesOfStatesUpdatedInRandomWalk] <<- proposedStates[,indicesOfStatesUpdatedInRandomWalk] 
				
		},

		############################################################
				# Visualisation


		showState = function( 
			iteration 	= 0L,
			type 		= 'initial states'
		)
		{
			tmpStates <- 
				as.data.frame( 
					getIteration( iteration, type ) 
				)

			colnames(tmpStates) <- temperatures
			rownames(tmpStates) <- 1:problemDimension

			return(tmpStates)
		},		


		############################################################
				# Algorithmic Methods


		getProposalLogsOfUnnormalisedDensities = function()
			#### Calculates logs of unnormalised densities in all the proposed states. 
		{
			return(
				log(
					apply( 
						proposedStates, 
						2, 
						targetDensity 
					)
				)	
			)
		},

		randomWalkProposal = function()
			#### Generates new current states.
		{
			proposedStates <<-	
				lastStates +
				ifelse(
					simpleCovariance,
					{
						proposalCovariancesCholeskised %*% 
						matrix( 
							rnorm( 
								n = problemDimension*noOfTemperatures 
							),
							nrow = problemDimension,
							ncol = noOfTemperatures
						)
					},
					sapply(
						1:noOfTemperatures,
						function( covarianceMatrixNo )
						{
							proposalCovariancesCholeskised[, 
								((covarianceMatrixNo-1)*problemDimension+1):
								(covarianceMatrixNo*problemDimension)
							] %*%
							rnorm(problemDimension)
						}
					)
				)

				# Now it will return proposed states log densities.
			return(	
				getProposalLogsOfUnnormalisedDensities()
			)
		},

		measureQuasiDistance = function(
			iState,
			jState
		)
		{
			return(
				quasiMetric(
					lastStates[,iState],
					lastStates[,jState]
				)
			)
		}
###########################################################################
				# Finis Structurae
	)
)