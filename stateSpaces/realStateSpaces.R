realStateSpace <- setRefClass(
	Class		= "RealStateSpaces",
	contains	= "StateSpaces",

###########################################################################
								# Fields

	fields		= list(

			## Dimension of the original state space of interest.
		spaceDim			= "integer",	

			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## Number of temperature levels.
		temperaturesNo		= "integer",

		# 	## The targetted density function, preserved by the algorithm's kernel.
		# targetDensity 		= "function",

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
		slotsNo 			= "integer",

			## Matrix containing covariances for the proposal kernel.
		proposalCovariancesCholeskised 	= "matrix",

			## Boolean: says whether proposal covariances are the same on different temperature levels.
		simpleProposalCovariance	= "logical",

		dataForPlot 		= "data.frame"	
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation


		initializeRealStateSpace = function(
			temperatures 		= numeric(0),
			temperaturesNo 		= 0L,
			spaceDim			= 0L,
#			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Initializes the real-state-space-specific fields.
		{
				# Checked already by the Simulation.
			temperatures 		<<- temperatures

			initialStatesDim	<- nrow(initialStates)
			initialStatesTemperaturesNo	<- ncol(initialStates)
			tmpSpaceDim			<- as.integer(spaceDim)
			tmpTemperaturesNo	<- temperaturesNo


			if( tmpSpaceDim==0L | tmpTemperaturesNo==0L )
			{
				if( initialStatesDim > 0L & 
					initialStatesTemperaturesNo > 0L ) 
				{
					lastStates		<<- initialStates

						# Consider input data representative.
					spaceDim 		<<- initialStatesDim
					temperaturesNo	<<- initialStatesTemperaturesNo

					cat('\nWe infered from the initial states that\n a) problem has dimension equal to ',spaceDim,'\n b) there are ',temperaturesNo,' chains to be consdered.\n')
				} else	
				cat("\nYou did not provide enough information to autogenerate initial states or did not provide the initial states yourself.\n")
			} else
			{
				spaceDim 		<<- tmpSpaceDim
				temperaturesNo 	<<- tmpTemperaturesNo

				if( tmpSpaceDim == initialStatesDim & 
					tmpTemperaturesNo == initialStatesTemperaturesNo )
				{	
					lastStates	<<- initialStates
				} else
				{
					if( initialStatesDim ==0 | initialStatesTemperaturesNo==0 )
					{		# No initial states supplied. Enough info to generate them.
						lastStates <<- 
							replicate( 
								n 	= temperaturesNo, 
								expr= runif( 
									n = spaceDim,
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
					nrow = spaceDim*(2*iterationsNo + 1), 
					ncol = temperaturesNo
				)

			freeSlotNo 		<<- 1L
			slotsNo 		<<- iterationsNo*2L + 1L

			insertStates()

			rm( tmpSpaceDim, tmpTemperaturesNo )

#			targetDensity	<<- targetDensity	
			quasiMetric 	<<- quasiMetric

			if(	class(proposalCovariances) == 'matrix' )
			{
				if(	nrow(proposalCovariances)==spaceDim &
					ncol(proposalCovariances)==spaceDim	)
				{
					proposalCovariancesCholeskised <<- 
						chol( proposalCovariances )			
					simpleProposalCovariance 	<<- TRUE
				} else
				{
					cat('You supplied a covariance matrix that does not conform to our state space dimension or did not care to supply it at all.\n Proceeding with identity covariances.\n')
					
					proposalCovariancesCholeskised <<- 
						diag(
							rep.int(1, times=spaceDim)
						)
					simpleProposalCovariance <<- TRUE
				}		
			} else
			{	
				if(	class(proposalCovariances) == 'list' &
					length(proposalCovariances)==temperaturesNo)
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
						simpleProposalCovariance 	<<- FALSE
					} else
					{
						cat('Your covariances are either not matrices or their size do not conform to problem dimension.\n Proceeding with identity covariances.\n')

						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=spaceDim)
							)
						simpleProposalCovariance <<- TRUE	
					}
				} else
				{
						cat('\nProposal covariances are not enlisted or are enlisted but the number of covariance matrices is other than the number of temperatures.\n Proceeding with identity covariances.\n')

						proposalCovariancesCholeskised <<- 
							diag(
								rep.int(1, times=spaceDim)
							)
						simpleProposalCovariance <<- TRUE	
				}		
			}	
		},


		initialize	= function(
			iterationsNo 		= 0L,  
			temperatures 		= numeric(0),
			temperaturesNo 		= 0L,
			spaceDim			= 0L,
#			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Splits the initialization to general state-space initialization and real-state-space-specific initialization.
		{
			initializeStateSpace(
				iterationsNo 		= iterationsNo
			)

			initializeRealStateSpace(
				temperaturesNo  	= temperaturesNo,
				temperatures 	  	= temperatures,
				spaceDim  			= spaceDim,
#				targetDensity 	 	= targetDensity,
				initialStates 	 	= initialStates,
				quasiMetric 	 	= quasiMetric,
				proposalCovariances = proposalCovariances
			)
		},


		############################################################
				# Data structure methods.


		simulationTerminated = function()
			#### Checks whether simulation is terminated.
		{
			return( freeSlotNo == slotsNo + 1L )
		},


		insertStates	= function() 
			#### Inserts current states to the data history (field: simulatedStates).
		{
			if( freeSlotNo <= slotsNo )
			{
				simulatedStates[
					((freeSlotNo-1)*spaceDim+1):
					(freeSlotNo*spaceDim),
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
			return(
				simulatedStates[ 
					((whichSlotNo-1)*spaceDim+1):
					(whichSlotNo*spaceDim),
				]
			)
		},


		getIteration	= function(
			iteration 	= 1L,
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


		showRealStateSpace = function()
		{
			cat('\nThe real-state-space inputs are here: \n')
			cat('Space dimension: ', spaceDim, '\n')
			cat('Number of temperatures: ', temperaturesNo, '\n')
			
			cat("Temperatures:\n")
			print(temperatures)
			cat("\n")

			cat('Initial States:\n')
			print( showState() )
			cat("\n")

			cat('Proposal covariances after Cholesky decomposition:\n')
			print( proposalCovariancesCholeskised )
			cat("\n")

			if( simulationTerminated() )
			{
				cat('Genarated states:\n')
				print( proposalCovariancesCholeskised )
				cat("\n")

				print( plotBaseTemperature() )
			}
		},

		show 	 = function()
		{
			showStateSpace()
			showRealStateSpace()
		},

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
			rownames(tmpStates) <- 1:spaceDim

			return(tmpStates)
		},		


		prepareDataForPlot = function()
			#### Reshuffles the entire history of states so that the entire result conforms to the data frame templates of ggplot2.
		{
			if (spaceDim == 2)	
			{			
				data  	<- vector(	"list", slotsNo )

				for( slotNo in 1:slotsNo )
				{
					data[[ slotNo ]] <-
						cbind(
							t( getStates( slotNo ) ),
							temperatures,
							rep.int( slotNo %/% 2, temperaturesNo ),
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
								temperaturesNo 
							)
						)
				}

				data 	<- as.data.frame( do.call( rbind, data ) )

				names( data )	<- 
					c("x","y","Temperature","Progress","Phase")

				data$Progress 	<- data$Progress/iterationsNo

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
			if ( spaceDim == 2 )
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
			if ( spaceDim == 2 )
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


		proposeLogsOfUMeasures = function()
			#### Calculates logs of unnormalised densities in all the proposed states. 
		{
			return(
				log(
					apply( 
						proposedStates, 
						2, 
						function( proposedState ) 
							targetMeasure$measure( 
								proposedState = proposedState
							) 
					)
				)	
			)
		},


		randomWalkProposal = function()
			#### Generates new current states and logs of their unnormalised densities.
		{
			proposedStates <<-	
				lastStates +
				if(	simpleProposalCovariance )
				{
					proposalCovariancesCholeskised %*% 
					matrix( 
						rnorm( 
							n = spaceDim*temperaturesNo 
						),
						nrow = spaceDim,
						ncol = temperaturesNo
					)
				} else
				{
					sapply(
						1:temperaturesNo,
						function( covarianceMatrixNo )
						{
							proposalCovariancesCholeskised[, 
								((covarianceMatrixNo-1)*spaceDim+1):
								(covarianceMatrixNo*spaceDim)
							] %*%
							rnorm(spaceDim)
						}
					)
				}
				
				# Now it will return proposed states log densities.
			return(	
				proposeLogsOfUMeasures()
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