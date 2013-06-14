realStateSpace <- setRefClass(
	Class		= "realStateSpace",
	contains	= "stateSpace",

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
		lastStates 			= "matrix",

			## Quasi metric between two points from the state space.
		quasiMetric  		= "function",

			## The number of the last cell complex in the data metrix where something was inserted. If zero then nothing was inserted.
		freeSlotNo 			= "integer",

			## Number of cell complexes in the data structure
		noOfSlots 			= "integer",

			## Matrix containing covariances for the proposal kernel.
		proposalCovariancesCholeskised 	= "matrix",

			## Boolean: says whether proposal covariances are the same on different temperature levels.
		simpleCovariance	= "logical",

		dataForPlot 			= "data.frame"	
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation


		initializeRealStateSpace = function(
			temperatures 		= numeric(0),
			noOfTemperatures 	= 0L,
			problemDimension	= 0L,
			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Initializes the real-state-space-specific fields.
		{
				# Checked already by the Simulation.
			temperatures 	<<- temperatures

			initialStatesDimension 	<- nrow(initialStates)
			initialStatesnoOfTemperatures	<- ncol(initialStates)
			tmpProblemDimension		<- as.integer(problemDimension)
			tmpNoOfTemperatures		<- noOfTemperatures


			if( tmpProblemDimension==0L | tmpNoOfTemperatures==0L )
			{
				if( initialStatesDimension > 0L & 
					initialStatesnoOfTemperatures > 0L ) 
				{
					lastStates	<<- initialStates

						# Consider input data representative.
					problemDimension<<- initialStatesDimension
					noOfTemperatures 		<<- initialStatesnoOfTemperatures

					cat('\nWe infered from the initial states that\n a) problem has dimension equal to ',problemDimension,'\n b) there are ',noOfTemperatures,' chains to be consdered.\n')
				} else	
				stop("\nYou did not provide enough information to autogenerate initial states or did not provide the initial states yourself. Good job! You're really getting easily into troubles now...\n")
			} else
			{
				problemDimension<<- tmpProblemDimension
				noOfTemperatures<<- tmpNoOfTemperatures

				if( tmpProblemDimension == initialStatesDimension & 
					tmpNoOfTemperatures == initialStatesnoOfTemperatures )
				{	
					lastStates	<<- initialStates
				} else
				{
					if( initialStatesDimension ==0 | initialStatesnoOfTemperatures==0 )
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
					} else
							# Inconsistency...
					stop("\nThe initial states you supplied differ in dimension or differ in number of chains.\n")
				}
			}
	
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

			if(	class(proposalCovariances) == 'matrix' )
			{
				if(	nrow(proposalCovariances)==problemDimension &
					ncol(proposalCovariances)==problemDimension	)
				{
					proposalCovariancesCholeskised <<- 
						chol( proposalCovariances )			
					simpleCovariance 	<<- TRUE
				} else
				{
					cat('You supplied a covariance matrix that does not conform to our state space dimension or did not care to supply it at all.\n Proceeding with identity covariances.\n')
					
					proposalCovariancesCholeskised <<- 
						diag(
							rep.int(1, times=problemDimension)
						)
					simpleCovariance <<- TRUE
				}		
			} else
			{	
				if(	class(proposalCovariances) == 'list' &
					length(proposalCovariances)==noOfTemperatures)
				{
					if (
						all(
							sapply(
								proposalCovariances,
								function( x ) 
									ifelse( 
										(class(x) == 'matrix'), 
										nrow(x)==2 & ncol(x)==2, 
										FALSE
									) 
							)
						)
					)	
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
					} else
					{
						cat('Your covariances are either not matrices or their size do not conform to problem dimension.\n Proceeding with identity covariances.\n')

						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=problemDimension)
							)
						simpleCovariance <<- TRUE	
					}
				} else
				{
						cat('\nProposal covariances are not enlisted or are enlisted but the number of covariance matrices is other than the number of temperatures.\n Proceeding with identity covariances.\n')

						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=problemDimension)
							)
						simpleCovariance <<- TRUE	
				}		
			}	
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
			#### Splits the initialization to general state-space initialization and real-state-space-specific initialization.
		{
			initializeStateSpace(
				noOfIterations 		= noOfIterations
			)

			initializeRealStateSpace(
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
			if( freeSlotNo <= noOfSlots )
			{
				simulatedStates[
					((freeSlotNo-1)*problemDimension+1):
					(freeSlotNo*problemDimension),
				] 	<<- lastStates

				freeSlotNo 	<<- freeSlotNo + 1L
			} else
			stop('The computer tried to make more steps than the user wanted him to. That is truly weird...') 
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


		getIteration	= function(
			iteration 	= 0L,
			type		= 'initial states' 
		)
			#### For a given iteration extracts results of a given step type, to choose among 'initial states', 'random walk', and 'swap'.
		{
			if ( simulationTerminated() )
			{
				result 	<- getStates(
					ifelse(
						type == 'initial states',
						1L,
						ifelse( 
							type == 'random walk',
							2L*iteration,
							ifelse(
								type = 'swap',
								2L*iteration+1L,
								stop("Error: you can choose only among types such as 'initial states', 'random walks', or 'swap'. " )
							)
						)
					)	
				)			
			} else
			{
				cat('Simulation not yet terminated - here are the initial states:\n\n')
				result 	<- getStates(1L) 
			}
			return( result )
		},


		updateStatesAfterRandomWalk = function(
			anyUpdate,
			indicesOfStatesUpdatedInRandomWalk	
		)
			#### Performs an update to the most recent values of the state space after the rejection step of the random walk phase.
		{
			if( anyUpdate )
			{
				lastStates[,indicesOfStatesUpdatedInRandomWalk] <<-
					proposedStates[,indicesOfStatesUpdatedInRandomWalk] 
			} 		
			insertStates()				
		},

				
		updateStatesAfterSwap = function(
			proposalAccepted,
			transposition	
		)
			#### Performs an update to the most recent values of the state space after the rejection step of the random swap phase.
		{
			if( proposalAccepted )	
			{
				lastStates[,transposition] <<- lastStates[,transposition[2:1]]
			}
						
			insertStates()	
		},				


		############################################################
				# Visualisation


		showState = function( 
			iteration 	= 0L,
			type 		= 'initial states'
		)
			#### Shows in a nice way a given iteration of the algorithm.
		{
			tmpStates <- 
				as.data.frame( 
					getIteration( iteration, type ) 
				)

			colnames(tmpStates) <- temperatures
			rownames(tmpStates) <- 1:problemDimension

			return(tmpStates)
		},		


		prepareDataForPlot = function()
			#### Reshuffles the entire history of states so that the entire result conforms to the data frame templates of ggplot2.
		{
			if (problemDimension == 2)	
			{			
				data  	<- vector(	"list", noOfSlots )

				for( slotNo in 1:noOfSlots )
				{
					data[[ slotNo ]] <-
						cbind(
							t( getStates( slotNo ) ),
							temperatures,
							rep.int( slotNo %/% 2, noOfTemperatures ),
							rep.int( 
								ifelse(
									slotNo == 1, 
									0,
									ifelse(
										slotNo %% 2 == 0,
										1,
										2
									)	
								), 
								noOfTemperatures 
							)
						)
				}

				data 	<- as.data.frame( do.call( rbind, data ) )

				names( data )	<- 
					c("x","y","Temperature","Progress","Phase")

				data$Progress 	<- data$Progress/noOfIterations

				data$Phase 	<- factor( data$Phase )

				levels( data$Phase ) <- c("Initial State","Random Walk","Swap")

				data$Temperature<- 
					factor( 
						data$Temperature,
						levels 	= temperatures,
						ordered	= TRUE  
					)

				dataForPlot <<- data 
			}
		}, 


		plotAllChains = function()
			#### Performs a plot of all simulated chains with an overlayed map of the real density from the Liang example.
		{
			if ( problemDimension == 2 )
			{
				require( ggplot2 )

				return( 
					qplot(
						x, 
						y, 
						data 	= dataForPlot,
						colour 	= Temperature
					) + 
					geom_point() +
					scale_colour_brewer(
						type 	= "seq", 
						palette = 3
					) +
					stat_contour(
						data = as.data.frame(
							read.csv2(
								"./Data/Liang_Density_Values_For_Contour_gg2plot.csv"
							)
						), 
						aes(x, y, z =z ), 
						bins	= 10, 
						size	= .5, 
						colour 	= "orange"
					) +
					ggtitle( "Parallel Tempering" ) +
					labs( x="", y="" )
				)		
			} else 
				cat( "\n It is highly non-trivial to plot a non-2D example \n.")
		},


		plotBaseTemperature = function()
			#### Performs a plot of the base level temperature chain of main interest with an overlayed map of the real density from the Liang example.

		{
			if ( problemDimension == 2 )
			{
				require( ggplot2 )

				return(
					qplot(
						x, 
						y, 
						data 	= dataForPlot[dataForPlot$Temperature==1,],
						colour 	= Progress
					) + 
					geom_point() +
					scale_colour_gradient(
						limits 	= c(0, 1),
						low		= "white",
						high 	= "black"
					) +
					stat_contour(
						data 	= as.data.frame(
							read.csv2(
								"./Data/Liang_Density_Values_For_Contour_gg2plot.csv"
							)
						), 
						aes( x, y, z =z ), 
						bins 	= 5, 
						size 	= .5, 
						colour 	= "red"
					) +
					ggtitle( "Parallel Tempering - Base Temperature" ) +
					labs( x="", y="" )
				)
			} else 
				cat( "\n It is highly non-trivial to plot a non-2D example \n.")
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
			#### Generates new current states and logs of their unnormalised densities.
		{
			proposedStates <<-	
				lastStates +
				if(	simpleCovariance )
				{
					proposalCovariancesCholeskised %*% 
					matrix( 
						rnorm( 
							n = problemDimension*noOfTemperatures 
						),
						nrow = problemDimension,
						ncol = noOfTemperatures
					)
				} else
				{
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
				}
				
				# Now it will return proposed states log densities.
			return(	
				getProposalLogsOfUnnormalisedDensities()
			)
		},

		measureQuasiDistance = function(
			iState,
			jState
		)
			#### Measure the quasi distance between states.
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
