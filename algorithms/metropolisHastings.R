metropolisHastings <- setRefClass(
	Class		= "MetropolisHastings",
	contains	= "Algorithms",

###########################################################################
								# Fields

	fields		= list(

			## Number of chains: independent random walk simulations.	
		chainsNo	= "integer",	

			## Names for the columns with different chains simulations.
		chainNames  = "character",

			## Vector with logs of unnormalised densities evaluated at current states. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure.
		lastStatesLogUDensities	= "numeric",

			## Vector with logs of unnormalised densities evaluated at current proposal. They are not yet multiplied by the inverse temperatures: this vector does not correspond to evaluation of the tensorised target measure. 

			#This can be turned into a local variable.
		proposalLogUDensities	= "numeric",

			## Vector with boolean values. TRUE when a state did get updated in either Random Walk phase or Swap phase of the algorithm.
		updatedStates			= "logical",	

			## A vector of integers: overall number of accepted random-walk proposals.
		acceptedRandomWalksNo	= "integer",

			## A vector of integers: overall number of accepted random-walk proposals.
		rejectedRandomWalksNo	= "integer",

			## Triggers detailed output during the simulation.
		detailedOutput			= "logical"
	),

###########################################################################
								# Methods

	methods 	= list(	


		############################################################
				# Initialisation
				
		initialize = function(
			iterationsNo 	= 0L,
			burnIn 			= 2000L,
			chainsNo 	  	= 0L,
			detailedOutput	= FALSE,
			...
		)
			#### Splits the initialization to general Simulations initialization and parallel-tempering-specific initialization.
		{
			if (!is.null(iterationsNo)){
			
				callSuper(
					iterationsNo    = iterationsNo,
					burnIn 			= burnIn,
					... 
				)
	
				chainsNo 				<<- chainsNo
				acceptedRandomWalksNo	<<- rep.int(0L, times = chainsNo)
				rejectedRandomWalksNo	<<- rep.int(0L, times = chainsNo)
				detailedOutput			<<- detailedOutput				
			}
		},


		insertChainNames = function() {
			tmpNames <- character( chainsNo )

			for ( i in 1:chainsNo ) {
				tmpNames[i] <- paste( 'chain ', i, sep="", collapse="" )
			}

			chainNames <<- tmpNames
		},


		prepareSimulation = function()
			#### Initialises values needed before the simulation.
		{
				# Initially everything is new.
			updatedStates <<- rep( TRUE, chainsNo)

				# Current states must get at least once calculated all without any updates.
			lastStatesLogUDensities <<-  
				stateSpace$proposeLogsOfUMeasures()
		},

		############################################################
				# Visualisation

		show	= function( ... )
			#### Calls the father-class show method followed by its own show method.
		{
			anteSimulationShow()
			insertChainNames()
			postSimulationShow()			
		},


		anteSimulationShow = function(...)
		{
			cat('\nUsing Metropolis-Hastings Algorithm\n')
			cat('\tNumber of steps: ', iterationsNo, '\n')		
		},


		acceptanceRejection = function(){

			return(  
				rbind( 
					chainNames, 
					acceptedRandomWalksNo/iterationsNo, 
					rejectedRandomWalksNo/iterationsNo
				)
			)
		},


		randomWalkHistory = function(){

			return(  
				acceptedRandomWalksNo/iterationsNo
			)
		},


		postSimulationShow = function(...)
		{
			if( simulationFinished )
			{
				cat("\tPercentage of accepted-rejected random-walks:\n")
				

				acceptance <- as.data.frame( acceptanceRejection )
				row.names(acceptance) 	<- 
					c("chain","accepted", "rejected")
				colnames(acceptance) 	<- 1:chainsNo
				print( acceptanceRejection() )
				cat("\n")
			}	
		},


		writeInfo = function(
	 		directoryToWrite,
	 		...
	 	)	
	 	{
	 		callSuper(
	 			directoryToWrite,
	 			...
	 		)

	 		write.csv2(
				acceptanceRejection(),
				file = paste(
					directoryToWrite,
					"/acceptedRandomWalks.csv",
					sep="",
					collapse=""
				),
				row.names 	= FALSE
			)
	 	},

		############################################################
				# Algorithmic Methods
					

		makeStepOfTheAlgorithm	= function( 
			iteration
		)
			#### Makes one step of random walk followed by one swap step.
		{
			if( detailedOutput ) 
				cat("===============================================================\n",
				"Random Walk No ", 	iteration,	'\n')
			
			randomWalk()

			if ( notBurning )
			{
				stateSpace$updateApproximatedIntegral( iteration )
			}
		},
		
			###### random walk sphere ######

		randomWalk = function()
			#### Performs the random walk step: it asks the state-space to generate the logs of unnormalised probabilities evaluated in the proposed points and then performs the usual rejection part. All this could be done parallely if it was needed - this feature will be shipped with version 2.0.
		{
			proposalLogUDensities <<- 		
				stateSpace$randomWalkProposal()

			if ( detailedOutput ) 
				cat(
					"\nPrevious Log Densities:\n",
					lastStatesLogUDensities,
					"\nProposal Log Densities:\n",
					proposalLogUDensities,
					"\n"
				)
			
			randomWalkRejection()
			
			triggerUpdateAfterRandomWalk() # both logdensities and current states.
		},


		randomWalkRejection = function()
			#### Here the Hastings quotients get compared with randomly generated values from the unit interval. All values are taken in logs for numerical stability.
		{
			Ulog 		<- log( runif( chainsNo ) )

			logAlpha 	<- getLogAlpha()

			updatedStates <<- Ulog < logAlpha


			if ( detailedOutput ) 
			{
				cat(
					"\nLast states log udensities: \n",
					lastStatesLogUDensities,
					"\nQuantities to be Compared with Log Uniform RV:\n",
					logAlpha,
					"\nlog(U):\n",
					Ulog,
					"\n",
					"\nUpdated Steps:\n",
					updatedStates,
					"\n"
				)
			}	
		},


		getLogAlpha = function()
		{
			return( proposalLogUDensities - lastStatesLogUDensities )
		},


		triggerUpdateAfterRandomWalk = function()
			#### This procedure updates the state space after an operation consisting of accepting any new proposal in the random-walk phase of the algorithm. Updates are also needed in the probabilities of the last states stored in a field in the parallel-tempering object.
		{
			anyUpdate 		<- 	any( updatedStates )

			if ( anyUpdate ) 	updateAfterRandomWalk()

			if ( notBurning & !all( updatedStates ) ){
				rejectedRandomWalksNo[ !updatedStates ] <<- 
					rejectedRandomWalksNo[ !updatedStates ] + 1L
			}

			stateSpace$updateStatesAfterRandomWalk( anyUpdate, updatedStates )
		},


		updateAfterRandomWalk = function()
		{
				# Updating state space..
			lastStatesLogUDensities[ updatedStates ]<<-	
				proposalLogUDensities[ updatedStates ]

			if ( notBurning ){	
				acceptedRandomWalksNo[ updatedStates ] 	<<-
					acceptedRandomWalksNo[ updatedStates ] + 1L
			} 	
		}
		
####################################################################
				# Finis Structurae
	)
)

								# No adaptation for now.
# metropolisHastings$lock( 
# 	'chainsNo'
# 
# )