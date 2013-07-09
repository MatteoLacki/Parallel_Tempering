realTemperedStateSpace <- setRefClass(
	Class		= "RealTemperedStateSpaces",
	contains	= "RealStateSpaces",

###########################################################################
								# Fields

	fields		= list(
#<fields>
			## Temperature levels for the parallel tempering algorithm.
		temperatures		= "numeric",

			## Quasi metric between two points from the state space.
		quasiMetric  		= "function"
#</fields>		
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation

#<method>
		initializeTemperedRealStateSpace = function(
			temperatures 		= numeric(0),
			quasiMetric 		= function(){} 
		)
			#### Initialises temperatures and quasi-metric.
		{
			insertTemperatures( 
				temperatures = temperatures 
			)
			quasiMetric <<- quasiMetric
		},

#<method>
		initialize	= function(
			iterationsNo 		= 0L,  
			chainsNo 			= 0L,
			spaceDim			= 0L,
			initialStates 		= matrix(ncol=0, nrow=0),
			proposalCovariances = matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			temperatures 		= numeric(0)
		)
			#### Splits the initialization to general state-space initialization and real-state-space-specific initialization.
		{
			initializeStateSpace(
				iterationsNo 		= iterationsNo
			)
			
			initializeRealStateSpace(
				chainsNo  			= chainsNo,
				spaceDim  			= spaceDim,
				initialStates 	 	= initialStates,
				proposalCovariances = proposalCovariances
			)
			
			initializeTemperedRealStateSpace(
				temperatures 	  	= temperatures,
				quasiMetric 	 	= quasiMetric
			)

			createDataStorage()
		},

		createDataStorage = function()
		{
			simulatedStates	<<- 
				matrix(
					nrow = spaceDim*(2L*iterationsNo + 1L), 
					ncol = chainsNo
				)

			freeSlotNo 		<<- 1L
			slotsNo 		<<- iterationsNo*2L + 1L
		},


		insertTemperatures = function(
			temperatures
		)
		{
				tmpTemperaturesNo <- length( temperatures )

				if ( tmpTemperaturesNo != chainsNo ) {
					stop('The number of supplied temperatures does not match the number of chains. Verify, if the ')
				} else {
					temperatures <<- temperatures
				}

		},


		############################################################
				# Data structure methods.


#<method>
		getIteration = function(
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

			colnames(result) <- temperatures
			rownames(result) <- 1:spaceDim	

			return( result )
		},


#<method>				
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

#<method>
		showRealTemperedStateSpace = function()
		{
			cat("Temperatures:\n")
			print(temperatures)
			cat("\n")

			cat('The Quasi-Metric:\n')
			print( quasiMetric )
			cat("\n")
		},

#<method>
		show = function()
		{
			showStateSpace()
			showRealStateSpace()
			showRealTemperedStateSpace()
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