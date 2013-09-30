targetMatteoDensity <- setRefClass(
	Class		= "TargetMatteoDensities",
	contains	= "TargetLiangDensities",

###########################################################################
								# Fields
	fields		= list(
			## matrix with column with means and sigma and weights.
		meansSigmasWeights = "matrix"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function(
			iterationsNo 			= NULL,
			quantileSimulationsNo 	= 10000,
			mixturesNo 				= 2L,
			# mixturesWeight			= c( .5, .5),
			mixturesWeight			= c(1/10, 9/10),
			mixturesMeans 			= matrix(
				c(2, 8, 2, 8), 
				nrow=2, 
				ncol=2, 
				byrow=TRUE
			),
			sigma 					= c(.7, .05),
			weightConstant 			=  1/( 2*pi),
			algorithmName  			= "Matteo",
			...
		)
		{
			if( !is.null(iterationsNo) ){

				callSuper(
					iterationsNo 	= iterationsNo,	
					quantileSimulationsNo = quantileSimulationsNo,
					mixturesNo 		= mixturesNo,
					mixturesWeight 	= mixturesWeight,
					mixturesMeans 	= mixturesMeans,
					sigma 			= sigma,
					weightConstant 	= weightConstant,
					algorithmName 	= algorithmName,
					...
				)

				meansSigmasWeights <<- rbind(
					mixturesMeans, 
					sigma, 
					mixturesWeight
				)
			}	
		},

		############################################################
				# Visualisation

		show = function()
		{
			cat('\nThe Matteo target density inputs are here: \n')
			cat('Mixture number: ', mixturesNo, '\n')
			cat("First Mixture weight: ", mixturesWeight[1], '\n')	
			cat("Second Mixture weight: ", mixturesWeight[2], '\n')	
			cat('First Mixture variance: ', sigma2[1], '\n')
			cat('Second Mixture variance: ', sigma2[2], '\n')
			cat("Mixtures' means:\n")
			print(mixturesMeans)
			cat('\n\n')
		},		

		############################################################
				# Algorithmic Methods				

		measure 	= function(
			proposedState
		)
		{
			return(
				sum(
					apply(
						meansSigmasWeights,
						2,
						function( b )
						{	
							b[4]*exp( 
							- crossprod( proposedState - b[1:2] )/ (2 * b[3]^2) 
							)/(b[3]^2)
						}
					)
				)*weightConstant
			)
		},


		distribuant = function( 
			x
		){

			weightedProbabilities <- 
				apply(
					meansSigmasWeights,
					2,
					function( b )
					{
						b[4]*pnorm( (x - b[1:2])/b[3] ) 
					}
				)

			return( sum(weightedProbabilities) )	
		},


		marginalDistribuant	= function(
			proposedState,
			coordinateNo
		){
			return(
				sum(
					apply(
						meansSigmasWeights,
						2,
						function( b )
						{
							b[4]*pnorm( (proposedState - b[coordinateNo])/b[3])
						}
					)	
				)	
			)
		},


		getFirstAndSecondMoments = function(){
			return(
				list(
					EX 	= sum(mixturesWeight*mixturesMeans[1,]),
					EY 	= sum(mixturesWeight*mixturesMeans[2,]),
					EX2 = sum(mixturesWeight*(sigma2+mixturesMeans[1,]^2)),
					EY2 = sum(mixturesWeight*(sigma2+mixturesMeans[2,]^2)),
					EXY = as.numeric(
						crossprod(
							mixturesWeight*mixturesMeans[1,],
							mixturesMeans[2,]
						)
					)
				)
			)
		}

####################################################################
				# Finis Structurae		
	)
)