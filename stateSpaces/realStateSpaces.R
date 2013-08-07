realStateSpace <- setRefClass(
	Class		= "RealStateSpaces",
	contains	= "StateSpaces",

###########################################################################
								# Fields

	fields		= list(

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

			## The number of the last cell complex in the data metrix where something was inserted. If zero then nothing was inserted.
		freeSlotNo 			= "integer",

			## Number of cell complexes in the data structure
		slotsNo 			= "integer",

			## Matrix containing covariances for the proposal kernel.
		proposalCholCovariances 	= "matrix",

			## Boolean: says whether proposal covariances are the same on different temperature levels.
		simpleProposalCovariance	= "logical",

			## Dataframe containing data that can be manipulated by the ggplot2.
		dataForPlot 		= "data.frame",

		ecdfData	 		= "data.frame"				
	),	
	
###########################################################################
								# Methods

	methods 	= list(

				
		############################################################
				# Initialisation

		initialize	= function(
			iterationsNo 		= NULL,  
			chainsNo 			= 0L,
			spaceDim			= 0L,
			initialStates 		= matrix(ncol=0, nrow=0),
			proposalCovariances = matrix(ncol=0, nrow=0),
			...
		)
			#### Splits the initialization to general state-space initialization and real-state-space-specific initialization.
		{
			if( !is.null(iterationsNo) ){			
				callSuper(
					iterationsNo 		= iterationsNo
				)

				spaceName 	 <<- 'Real State Space'
	
				insertInitialStates( 
					initialStates 	= initialStates,
					spaceDim 		= spaceDim,
					chainsNo		= chainsNo
				)				

				if( spaceDim > 0 & chainsNo > 0 ){	createDataStorage() }			
				if( length(slotsNo) > 0 ){	insertStates()	}

				insertProposalCovariances(
					proposalCovariances = proposalCovariances
				)			
			}
		},


		insertInitialStates	= function( 
			initialStates 		= matrix(ncol=0, nrow=0),
			spaceDim			= 0L,
			chainsNo			= 0L
			)
		{
			tmpSpaceDim 			<- as.integer(spaceDim)
			tmpChainsNo 			<- as.integer(chainsNo)
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
		){
			switch(
				class(proposalCovariances),
				'matrix'	= {
					if( checkCovariance(proposalCovariances) )
					{
						proposalCholCovariances 	<<- 
							chol( proposalCovariances )	

						simpleProposalCovariance 	<<- TRUE

					} else {
						cat('You supplied a covariance matrix that does 
							not conform to our state space dimension 
							or did not care to supply it at all.\n 
							Proceeding with identity covariances.\n')
						
						proposalCholCovariances <<- 
							diag( rep.int(1, times=spaceDim) )

						simpleProposalCovariance<<- TRUE
					}		
				},
				'list'		= {
					if( 
						all(
							sapply(
								proposalCovariances,
								checkCovariance	
							)
						) & 
						length(proposalCovariances) == chainsNo
					){
						proposalCholCovariances <<-
							do.call( 
								cbind, 
								lapply( 
									proposalCovariances, 
									chol 
								) 
							)

						simpleProposalCovariance 	<<- FALSE

					} else {
						cat('Your covariances are either not matrices 
							or their size do not conform to problem dimension
							or you provided a number of them that is different
							from the number of temperatures.\n\n
							Proceeding with identity covariances.\n' 
						)

						proposalCholCovariances <<- 
							diag( rep.int(1, times=spaceDim) )
							
						simpleProposalCovariance<<- TRUE
					}
				},	
				stop('Wrong format of the proposal covariances.')
			)
		},


		checkCovariance = function(
			covarianceMatrix
		){
			return(
				ifelse(
					class(covarianceMatrix) == 'matrix',
					nrow( covarianceMatrix )==spaceDim & 
					ncol(covarianceMatrix )	==spaceDim, 
					FALSE
				)
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
			cat('The computer tried to make more steps than the user wanted him to. That is truly weird...') 
		},


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

			colnames(result) <- tmpNames
			rownames(result) <- 1:spaceDim	

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


		############################################################
				# Visualisation

		show = function(
			algorithmName,
			...
		){
			anteSimulationShow()
			postSimulationShow(algorithmName = algorithmName)	
		},


		anteSimulationShow = function(...)
		{
			cat('\nUsing', spaceName ,'.\n')
			cat('\tNumber of iterations of the algorithm: ', iterationsNo, '\n')
			cat('\tSpace dimension: ', spaceDim, '\n')
			cat('\tNumber of chains: ', chainsNo, '\n')
			
			cat('\tInitial States:\n')
			print( getIteration() )
			cat("\n")

			cat('\tProposal covariances after Cholesky decomposition:\n')
			print( proposalCholCovariances )
			cat("\n")		
		},


		postSimulationShow = function(
			algorithmName,
			...
		)
		{
			if( simulationTerminated()) 
			{
				print( plotBasics( algorithmName = algorithmName ) )
			}
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
			#### Reshuffles the entire history of states so that the entire result conforms to the data frame templates of ggplot2
		{
			require( ggplot2 )

			switch(
				spaceDim,	
				'1L'= cat('To be implemented'),
				'2L'={
					data  	<- vector(	"list", slotsNo )

					for( slotNo in 1:slotsNo )
					{
						data[[ slotNo ]] <- 
							cbind(
								t( getStates( slotNo ) ),
								1:chainsNo,
								rep.int( slotNo, chainsNo ),
								ifelse( slotNo == 1, 0, 1)
							)
					}

					data <- 
						as.data.frame( do.call( rbind, data ) )

					names( data )		<- c("x","y","Chain","Progress","Phase")
					data$Progress 		<- data$Progress/iterationsNo
					data$Phase 			<- factor( data$Phase )
					levels( data$Phase )<- c("Initial State","Random Walk")
					
					dataForPlot <<- data 
				},
				cat("I do not know how to visualise 
					non-2D state-spaces")
			)		
		},


		plotBasics = function(
			algorithmName 
		)
			#### Performs a plot of the base level temperature chain of main interest with an overlayed map of the real density from the Liang example.
		{
			require( ggplot2 )
			contourData 	<- 	targetMeasure$realDensityValues 

			switch(
				spaceDim, 
				'1L'= cat('To be implemented'),
				'2L'={
					return(
						qplot(
							x, 
							y, 
							data 	= dataForPlot,
							colour 	= Progress
						) + 
						geom_point() +
						scale_colour_gradient(
							limits 	= c(0, 1),
							low		= "white",
							high 	= "black"
						) +
						stat_contour(
							data 	= contourData, 
							aes( x, y, z =z ), 
							breaks = targetMeasure$quantiles[1],
							size 	= .5, 
							colour 	= "yellow"
						) +
						stat_contour(
							data 	= contourData, 
							aes( x, y, z =z), 
							breaks = targetMeasure$quantiles[2],
							size 	= .5,
							colour 	= "orange"
						) +
						stat_contour(
							data 	= contourData, 
							aes( x, y, z =z), 
							breaks = targetMeasure$quantiles[3:5],
							size 	= .5,
							colour 	= "red"
						) +
						ggtitle( algorithmName ) +
						labs( x="", y="" )
					)
				},	
				cat( "\n It is highly non-trivial 
					to plot a non-2D example \n.")
			)
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
					proposalCholCovariances %*% 
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
							proposalCholCovariances[, 
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

		############################################################
				# Post-Simulation evaluation.

		initializeEcdfData 	= function(){

			require( sqldf )
			ecdfData 		<<- sqldf(
				"SELECT 	x, y, COUNT(*) AS charge 
				FROM 	dataForPlot 
					WHERE Temperature=1 AND 
					PHASE='Swap' 
				GROUP BY 	x, y;"
			)

			rowsNo	<- nrow(ecdfData)

			ecdfData$No   	<<- 1:rowsNo
			ecdfData$ecdf 	<<- rep.int(NA, times=rowsNo)
		},

		ecdf2 = function( 
			rows = 1:nrow(ecdfData)
		){

			if( length( rows ) == 1 ){
				ecdfData[rows,'ecdf'] <<- ecdfData[rows,'charge']
			} else {
				S 		<- ecdfData[rows,]
				rowsNo 	<- nrow(S)
				S$AorB 	<- rep.int(NA, times=rowsNo)
				
					# division step
				med 	<- median( S$x )
				cond	<- S$x <= med

					# recursive step
				A 	<- S$No[cond]
				B	<- S$No[!cond]
					
				S[cond,'AorB'] 	<- 'A'
				S[!cond,'AorB'] <- 'B'

				ecdf2(A)
				ecdf2(B)

					# marriage step

				account <- 0
				S 		<- S[order(S$y),]	

				for ( i in 1:rowsNo) switch(
					S[i,'AorB'],
					'A'={
						account <- account + S[i,'charge']
					},
					'B'={
						ecdfData[i, 'ecdf'] <<- ecdfData[i, 'ecdf'] + account
					},
					stop("Value out of bound. Very suspicious.")
				)
			}	
		}		


###########################################################################
				# Finis Structurae
	)
)