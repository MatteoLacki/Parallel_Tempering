targetMatteoDensity <- setRefClass(
	Class		= "TargetMatteoDensities",
	contains	= "TargetMeasures",

###########################################################################
								# Fields
	fields		= list(
#<fields>
			## Number of mixtures of gaussian variables. 
		mixturesNo 		= "integer",

			## Weights of every mixture.
		mixturesWeight  = "numeric",

			## The norming constant of the covariance matrices of the Matteo density.
		sigma2			= "numeric",

			## Square root of the norming constant of the covariance matrices of the Matteo density.
		sigma			= "numeric",

			## A constant related to weight and sigma. 
		weightConstant  = "numeric",

			## Mean values of the normal distributions that are getting mixed.
		mixturesMeans	= "matrix",

			## Approximated quantiles of the distribution. 
		quantiles 		= "numeric",	 

			## matrix with column with means and sigma and weights.
		meansSigmasWeights = "matrix"
#</fields>		
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation
#<method>
		initialize 	= function(
			quantileSimulationsNo = 10000
		)
		{
			mixturesNo 		<<- 2L

			mixturesWeight 	<<- c(1/10, 9/10)

			mixturesMeans 	<<- 
				matrix(
					c(2, 8, 2, 8), 
					nrow=2, 
					ncol=2, 
					byrow=TRUE
				)

			sigma 	<<- c(.7, .05)
			sigma2 	<<- sigma^2	

			meansSigmasWeights <<- rbind(mixturesMeans, sigma, mixturesWeight)

			weightConstant 	<<-  1/( 2*pi)

			establishTrueValues()

			simulateQuantiles( simulationsNo=quantileSimulationsNo  )
		},

		############################################################
				# Visualisation
#<method>
		show = function()
		{
			cat('\nThe Matteo target density inputs are here: \n')
			cat('Mixture number: ', mixturesNo, '\n')
			cat("First Mixture weight: ", mixturesWeight[1], '\n')	
			cat("Second Mixture weight: ", mixturesWeight[2], '\n')	
			cat('First Mixture variance: ', sigma2[1], '\n')
			cat('Second Mixture variance: ', sigma2[1], '\n')
			cat("Mixtures' means:\n")
			print(mixturesMeans)
			cat('\n\n')
		},		

#<method>		
		plotDistribuant = function()
		{
			grid  <- 
				getSquareGrid(
					minimum = -2,
					maximum = 12,
					mesh 	= 0.1
				)

			require("lattice")

			distribuantInGrid <-
				as.data.frame(
					cbind(
						t( grid ),
						apply(
							grid,
							2,
							function( gridPoint ) distribuant( gridPoint )	
						)
					)			
				)

			p <- wireframe(distribuantInGrid[,3] ~ tmpRealDistribuantValues[,1] * distribuantInGrid[,2])						

			return(p)
		},


		############################################################
				# Algorithmic Methods				
#<method>
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
							- crossprod( proposedState - b[1:2] )/ (2 * b[3]) 
							)
						}
					)
				)
			)
		},

#<method>
		establishTrueValues = function()
		{
			cat("\nEvaluating Matteo density example and saving it. This might take a while.\n\n")

			fileName <- "./data/MatteoTrueValues.csv"

			if( file.exists( fileName ) )
			{
				cat("\nFile already exists. Proceeding with loading it.\n\n")

				realDensityValues <<- 
					as.data.frame( 
						read.csv2(
							fileName,
							header = TRUE
							) 
						)
			} else 	
			{
				grid  <- 
					getSquareGrid(
						minimum = -2,
						maximum = 12,
						mesh 	= 0.1
					)

				tmpRealDensityValues <-
					as.data.frame(
						cbind(
							t( grid ),
							apply(
								grid,
								2,
								function( gridPoint ) measure( gridPoint )	
							)
						)			
					)

				colnames( tmpRealDensityValues ) <- c("x", "y", "z")
				 
				realDensityValues <<- tmpRealDensityValues


				if( !file.exists("./data") ) 	dir.create("./data")

				write.csv2(
					realDensityValues,
					file 		= fileName
				)
			}
		},

#<method>
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

#<method>
		getSquareGrid = function( 
			minimum , 
			maximum ,
			mesh 
		){
			gridBase 	<- seq( minimum, maximum, by = mesh)
			gridLength 	<- length( gridBase )

			return( 
				do.call(
					cbind,
					lapply(
						gridBase,
						function( gridBasePoint )
						{
							matrix( 
								c(
									rep.int( gridBasePoint, times= gridLength ),
									gridBase
								),
								ncol = gridLength,
								nrow = 2,
								byrow=TRUE
							)
						}
					)
				)
			)
		},


		getQuantiles = function(
			simulationsNo
		){
			X <- rbind(
				matrix(
					rnorm(n = 2*simulationsNo,mean =0,sd=sigma), 
					nrow=2, 
					ncol=simulationsNo
				),
				sample.int( 
					n=mixturesNo, 
					size=simulationsNo, 
					replace=TRUE
				) 	
			)

			X <- apply( X, 2, function(x) c(x[1], x[2]) + mixturesMeans[,x[3]] )

			X <- apply( X, 2, function(x) measure(x) )

			return( quantile(x=X, probs = c(.01, .05, .25, .5, .75)) )				
		},


		simulateQuantiles = function(
			simulationsNo
		){	
			cat("\nApproximating quantiles.\n\n")

			fileName <- paste(
				"./data/MatteoApproximateQuantiles_", 
				simulationsNo,
				'.csv', 
				collapse='',
				sep=''
			)

			if( file.exists( fileName ) )
			{
				cat("\nApproximation already carried out in the past. Proceeding with precalculated values.\n\n")

				tmp <- 	
					read.csv2(
						fileName,
						header = TRUE
					) 
					
				quantiles <<- tmp$x 

					
			} else 	
			{			
				if( !file.exists("./data") ) 	dir.create("./data")

					# Gets 3 independent approximations of the 3 quantiles .25, .5 , .75 .
				quantiles <<- diag( 
					replicate( 
						n=5, 
						getQuantiles( simulationsNo ) 
					) 
				)

				write.csv2(
					quantiles,
					file 		= fileName
				)
			}
		}


####################################################################
				# Finis Structurae		
	)
)