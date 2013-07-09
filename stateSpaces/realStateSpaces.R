realStateSpace <- setRefClass(
	Class		= "RealStateSpaces",
	contains	= "StateSpaces",

###########################################################################
								# Fields

	fields		= list(
#<fields>
			## Dimension of the original state space of interest.
		spaceDim			= "integer",	

			## Number of chains (independent simulations)
		chainsNo			= "integer",

			## Matrix containing all simulated points.
		simulatedStates		= "matrix",

			## Matrix containing proposals of the random walk.
		proposedStates 		= "matrix",

			## Matrix containing points simulated in the last step of the algorithm: after random walk phase it is composed of previous current states with updates being the accepted proposals.
		lastStates 			= "matrix",

		# 	## Quasi metric between two points from the state space.
		# quasiMetric  		= "function",

			## The number of the last cell complex in the data metrix where something was inserted. If zero then nothing was inserted.
		freeSlotNo 			= "integer",

			## Number of cell complexes in the data structure
		slotsNo 			= "integer",

			## Matrix containing covariances for the proposal kernel.
		proposalCovariancesCholeskised 	= "matrix",

			## Boolean: says whether proposal covariances are the same on different temperature levels.
		simpleProposalCovariance	= "logical",

			## Dataframe containing data that can be manipulated by the ggplot2.
		dataForPlot 		= "data.frame"	
#</fields>		
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation

#<method>
		initializeRealStateSpace = function(
			chainsNo 			= 0L,
			spaceDim			= 0L,
			initialStates 		= matrix(ncol=0, nrow=0),
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Initializes the real-state-space-specific fields.
		{
			insertInitialStates( 
				initialStates 	= initialStates,
				spaceDim 		= spaceDim,
				chainsNo		= chainsNo
			)				
			
			if (length(slotsNo) > 0) {
				insertStates()
			}
			
			insertProposalCovariances(
				proposalCovariances = proposalCovariances
			)
		},

#<method>
		initialize	= function(
			iterationsNo 		= 0L,  
			chainsNo 			= 0L,
			spaceDim			= 0L,
			initialStates 		= matrix(ncol=0, nrow=0),
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Splits the initialization to general state-space initialization and real-state-space-specific initialization.
		{
			initializeStateSpace(
				iterationsNo 		= iterationsNo
			)

			initializeRealStateSpace(
				initialStates 	 	= initialStates,
				spaceDim  			= spaceDim,
				chainsNo  			= chainsNo,
				proposalCovariances = proposalCovariances
			)

			if( spaceDim > 0 & chainsNo > 0 ) { createDataStorage() }
		},

#<method>
		insertInitialStates	= function( 
			initialStates 		= matrix(ncol=0, nrow=0),
			spaceDim			= 0L,
			chainsNo			= 0L
			)
		{
			tmpSpaceDim 	<- as.integer(spaceDim)
			tmpChainsNo 	<- as.integer(chainsNo)

			initialStatesDim 		<- nrow(initialStates)  
			initialStatesChainsNo	<- ncol(initialStates)

			if ( initialStatesDim > 0  & initialStatesChainsNo > 0 )
			{
				cat( "We infered state-space dimension and number of chains directly from the provided initial states.")

				spaceDim 	<<- initialStatesDim
				chainsNo  	<<- initialStatesChainsNo
				lastStates 	<<- initialStates

				if( initialStatesDim 		!= tmpSpaceDim | 
					initialStatesChainsNo 	!= tmpChainsNo )
				{
					cat("Thou have supplied us with inconsisten data. We proceed with the initial states information on the state-space.")
				}		
			} else {
				
				if ( tmpSpaceDim > 0 & tmpChainsNo > 0) {

					cat("You did not provide any initial states, so we will generate them uniformly from a hypercube [0,10]^dim")

					lastStates <<- replicate( 
						n 		= tmpChainsNo, 
						expr	= runif( n = tmpSpaceDim, min=0, max=10)
					)				

					spaceDim 	<<- tmpSpaceDim
					chainsNo 	<<- tmpChainsNo

				} else 
				cat("Not enough information on state-space dimension or the number of simulation chains.")
				
			}

			proposedStates 	<<- lastStates

		},


		createDataStorage = function()
		{
			simulatedStates	<<- 
			matrix(
				nrow = spaceDim*(iterationsNo + 1), 
				ncol = chainsNo
			)

			freeSlotNo 		<<- 1L
			slotsNo 		<<- iterationsNo + 1L
		},


		insertProposalCovariances = function(
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
		{
			if(	class(proposalCovariances) == 'matrix' )
			{
				if( nrow(proposalCovariances) > 0 ) {
				
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
				} else {
					cat('\nProposal covariances are not enlisted or are enlisted but the number of covariance matrices is other than the number of temperatures.\n')
				}
			} else
			{	
				if(	class(proposalCovariances) == 'list' &
					length(proposalCovariances)== chainsNo)
				{
					if (
						all(
							sapply(
								proposalCovariances,
								function( x ) 
									ifelse( 
										(class(x) == 'matrix'), 
										nrow(x)==spaceDim & ncol(x)==spaceDim, 
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
								rep.int(x =1, times=spaceDim)
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

		############################################################
				# Data structure methods.

#<method>
		simulationTerminated = function()
			#### Checks whether simulation is terminated.
		{
			return( freeSlotNo == slotsNo + 1L )
		},

#<method>
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
			cat('The computer tried to make more steps than the user wanted him to. That is truly weird...') 
		},

#<method>
		getStates = function(
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

#<method>
		getIteration = function(
			iteration 	= 1L,
			type 		= 'initial states' 
		)
		{
			if ( simulationTerminated() )
			{
				result <- getStates(
					ifelse(
						type == 'initial states',
						1L,
						iteration+1L
					)
				)
			} else {
				cat('Simulation not yet terminated - here are the initial states:\n\n')
				result 	<- getStates(1L) 
			}

			tmpNames <- character( chainsNo )

			for ( i in 1:chainsNo ) {
				tmpNames[i] <- paste( 'chain ', i, sep="", collapse="" )
			}

			colnames(tmpStates) <- tmpNames
			rownames(tmpStates) <- 1:spaceDim	

			return( result )
		},

#<method>
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


		############################################################
				# Visualisation

#<method>
		showRealStateSpace = function()
		{
			cat('\nThe real-state-space inputs are here: \n')
			cat('Space dimension: ', spaceDim, '\n')
			cat('Number of chains: ', chainsNo, '\n')
			
			cat('Initial States:\n')
			print( getIteration() )
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

#<method>
		show = function()
		{
			showStateSpace()
			showRealStateSpace()
		},

#<method>
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

#<method>
		# prepareDataForPlot = function()
		# 	#### Reshuffles the entire history of states so that the entire result conforms to the data frame templates of ggplot2.
		# {
		# 	if (spaceDim == 2)	
		# 	{			
		# 		data  	<- vector(	"list", slotsNo )

		# 		for( slotNo in 1:slotsNo )
		# 		{
		# 			data[[ slotNo ]] <-
		# 				cbind(
		# 					t( getStates( slotNo ) ),
		# 					temperatures,
		# 					rep.int( slotNo %/% 2, chainsNo ),
		# 					rep.int( 
		# 						ifelse(
		# 							slotNo == 1, 
		# 							0,
		# 							ifelse(
		# 								slotNo %% 2 == 0,
		# 								1,
		# 								2
		# 							)	
		# 						), 
		# 						chainsNo 
		# 					)
		# 				)
		# 		}

		# 		data 	<- as.data.frame( do.call( rbind, data ) )

		# 		names( data )	<- 
		# 			c("x","y","Temperature","Progress","Phase")

		# 		data$Progress 	<- data$Progress/iterationsNo

		# 		data$Phase 	<- factor( data$Phase )

		# 		levels( data$Phase ) <- c("Initial State","Random Walk","Swap")

		# 		data$Temperature<- 
		# 			factor( 
		# 				data$Temperature,
		# 				levels 	= temperatures,
		# 				ordered	= TRUE  
		# 			)

		# 		dataForPlot <<- data 
		# 	}
		# }, 

#<method>
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
						data = targetMeasure$realDensityValues, 
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

#<method>
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
						data 	= targetMeasure$realDensityValues, 
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

#<method>
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

#<method>
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
							n = spaceDim*chainsNo 
						),
						nrow = spaceDim,
						ncol = chainsNo
					)
				} else
				{
					sapply(
						1:chainsNo,
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
		}
###########################################################################
				# Finis Structurae
	)
)