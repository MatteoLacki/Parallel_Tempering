LiangRealStateSpace <- setRefClass(
	Class		= "LiangRealStateSpace",
	contains	= "realStateSpace",

###########################################################################
								# Fields

	fields		= list(

			## Number of mixtures of gaussian variables. 
		mixturesNo 		= "integer",

			## Weights of every mixture.
		mixturesWeight  = "numeric",

			## The norming constant of the covariance matrices of the Liang density.
		sigma2			= "numeric",

			## Square root of the norming constant of the covariance matrices of the Liang density.
		sigma			= "numeric",

			## A constant related to weight and sigma. 
		weightConstant  = "numeric",

			## Mean values of the normal distributions that are getting mixed.
		mixturesMeans	= "matrix"

	),	
	
###########################################################################
								# methods

	methods 	= list(

				
		############################################################
				# Initialisation

		initializeLiangRealStateSpace = function()
		{
			mixturesNo 		<<- 20

			mixturesWeight 	<<- 1/mixturesNo

			mixturesMeans 	<<- 
				matrix(
					c(2.18, 8.67, 4.24, 8.41, 3.93, 3.25, 1.70, 4.59, 6.91, 6.87, 5.41, 2.70, 4.98, 1.14, 8.33, 4.93, 1.83, 2.26, 5.54, 1.69, 5.76, 9.59, 8.48, 1.68, 8.82, 3.47, 0.50, 5.60, 5.81, 5.40, 2.65, 7.88, 3.70, 2.39, 9.50, 1.50, 0.09, 0.31, 6.86, 8.11), 
					nrow=Liang_Problem_Dimension, 
					ncol=20, 
					byrow=TRUE
				)

			sigma 	<<- .1
			sigma2 	<<- sigma^2	

			weightConstant 	<<-  mixturesWeight/( sigma*sqrt( 2* pi) )
		},		

		initialize	= function(
			temperatures 		= c(1, 2.8, 7.7, 21.6, 60),
			iterationsNo 		= 0L,  
			temperaturesNo 		= 5L,
			spaceDim			= 2L,
			targetDensity 		= function(){},
			initialStates 		= matrix(ncol=0, nrow=0),
			quasiMetric 		= function(){},
			proposalCovariances = matrix(ncol=0, nrow=0)
		)
			#### Splits the initialization to general state-space initialization, real-state-space specific initialization, and Liang-example specific .
		{
			initializeStateSpace(
				iterationsNo 	= iterationsNo
			)

			tmpProposalCovariances <- vector( "list", temperaturesNo )

			for (i in 1:temperaturesNo )
			{
				tmpProposalCovariances[[i]] <- 
					diag( temperatures[i]^2, nrow=2, ncol=2 ) 				
			}

			initializeRealStateSpace(
				temperaturesNo 	 = temperaturesNo,
				temperatures 	 = temperatures,
				spaceDim		 = spaceDim,
				targetDensity 	 = targetDensity,
				initialStates 	 = initialStates,
				quasiMetric 	 = quasiMetric,
				proposalCovariances = tmpProposalCovariances
			)

			initializeLiangRealStateSpace()
		},


		############################################################
				# Algorithmic Methods.

		getProposalLogsOfUnnormalisedDensities = function()
			#### Calculates logs of unnormalised densities in all the proposed states. 
		{
			return(
				log(
					apply( 
						proposedStates, 
						2, 
						evaluateLiangDensity() 
					)
				)	
			)
		},


		evaluateLiangDensity = function( 
			proposedState 
		)
		{
			return(
				sum(
					apply(
						mixturesMeans,
						2,
						function( mixtureMean )
						{	
							exp( 
								crossprod( proposedState - mixtureMean )/ (2 * sigma2) 
							)
						}
					)
				)*
				weightConstant
			)
		}
###########################################################################
				# Finis Structurae
	)
)
