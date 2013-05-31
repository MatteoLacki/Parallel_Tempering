
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

			## Matrix containing points simulated in the last step of the algorithm.
		currentStates 		= "matrix",

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
			...
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

			enlistedAdditionalInfo	<- list(...)

			cat('\nWhat is hidden in ...\n',names(enlistedAdditionalInfo),'\n')
			print('proposalCovariances' %in% names(enlistedAdditionalInfo))

			ifelse(
				(
					'proposalCovariances' %in% names(enlistedAdditionalInfo)
				),
				{
					print(length(enlistedAdditionalInfo$proposalCovariances))	
					tmpProposalCovariances <- 
						enlistedAdditionalInfo$proposalCovariances


					print(tmpProposalCovariances)
					print(class(tmpProposalCovariances))

					ifelse(
						( 	
							class(tmpProposalCovariances) == 'matrix' 	
						),
						{
							proposalCovariancesCholeskised <<- 
								chol( tmpProposalCovariances )
							
							print(proposalCovariancesCholeskised)
							simpleCovariance 	<<- TRUE
							print(simpleCovariance)
						},
						ifelse(
							(
								class(tmpProposalCovariances) == 'list' &
								length(tmpProposalCovariances)==noOfTemperatures
							),
							ifelse(
								all(
									sapply(
										tmpProposalCovariances,
										function(x) 
											ifelse( 
												(class(x) == 'matrix'), 
												nrow(x)==2 & ncol(x)==2, 
												FALSE
											) 
									)
								),	
								{
										### Could use completely different method here. Maybe create a specialise subobject.
									proposalCovariancesCholeskised <<-
										do.call( 
											cbind, 
											lapply( 
												tmpProposalCovariances, 
												chol 
											) 
										)

									simpleCovariance 	<<- FALSE
								},
								stop('Your covariances are either not matrices or their size do not conform to problem dimension.')
							),
							stop('\nProposal covariances are not enlisted or are enlisted but the number of covariance matrices is other than the number of temperatures.\n')
						)
					)
					cat('asd')
					#rm(	tmpProposalCovariances )
				},
				{
					cat('\nYou did not submit any proposal covariances or misspelled the name (should be proposalCovariances=..). \nProceeding with identity matrix for all temperatures.\n')

					proposalCovariancesCholeskised <<- 
						diag(
							rep.int(1, times=problemDimension)
						)

					simpleCovariance <<- TRUE
				}
			)
	
			cat('sdfs')
			rm( enlistedAdditionalInfo )
		},

		initialize	= function(
			temperatures 		= numeric(0),
			noOfIterations 		= 0L,  
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			...
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
				... 
			)
		},


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
					cat('Simulation not yet terminated - here are the initial states:\n\n')
					result 	<- getStates(1L) 
				}
			)
			return( result )
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