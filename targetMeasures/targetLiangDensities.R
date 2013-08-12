targetLiangDensity <- setRefClass(
	Class		= "TargetLiangDensities",
	contains	= "TargetMeasures",

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
		mixturesMeans	= "matrix",

			## Approximated quantiles of the distribution. 
		quantiles 		= "numeric"	 
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function(
			quantileSimulationsNo = 10000,
			...
		)
		{
			callSuper(...)

			mixturesNo 		<<- 20L

			mixturesWeight 	<<- 1/mixturesNo

			mixturesMeans 	<<- 
				matrix(
					c(2.18, 8.67, 4.24, 8.41, 3.93, 3.25, 1.70, 4.59, 6.91, 6.87, 5.41, 2.70, 4.98, 1.14, 8.33, 4.93, 1.83, 2.26, 5.54, 1.69, 5.76, 9.59, 8.48, 1.68, 8.82, 3.47, 0.50, 5.60, 5.81, 5.40, 2.65, 7.88, 3.70, 2.39, 9.50, 1.50, 0.09, 0.31, 6.86, 8.11), 
					nrow=2, 
					ncol=20, 
					byrow=TRUE
				)

			sigma 	<<- .1
			sigma2 	<<- sigma^2	

			# weightConstant 	<<-  mixturesWeight/( sigma*sqrt( 2* pi) )
			weightConstant 	<<-  mixturesWeight/( sigma2*2*pi )

			establishTrueValues()

			simulateQuantiles( simulationsNo=quantileSimulationsNo  )
		},

		############################################################
				# Visualisation

		show = function()
		{
			cat('\nThe Liang target density inputs are here: \n')
			cat('Mixture number: ', mixturesNo, '\n')
			cat("Mixtures' weight: ", mixturesWeight, '\n')	
			cat('Variance: ', sigma2, '\n')
			cat("Mixtures' means:\n")
			print(mixturesMeans)
			cat('\n\n')
		},		

		
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

		measure 	= function(
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
								- crossprod( proposedState - mixtureMean )/ (2 * sigma2) 
							)
						}
					)
				)*
				weightConstant
			)
		},


		establishTrueValues = function()
		{
			cat("\nEvaluating Liang-Wang density example and saving it. This might take a while.\n\n")

			fileName <- "./data/LiangTrueValues.csv"

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

		distribuant = function( 
			x
		){
			pnorms <- 
				apply(
					mixturesMeans,
					2,
					function( means )
					{
						pnorm( (x - means)/sigma )
					}
				)

			return( crossprod( pnorms[1,], pnorms[2,] )*mixturesWeight  )	
		},

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
			cat("\nApproximating real quantiles by Monte Carlo and saving them.\n\n")

			fileName <- paste(
				"./data/LiangApproximateQuantiles_", 
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