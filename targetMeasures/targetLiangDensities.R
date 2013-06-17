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
		mixturesMeans	= "matrix"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function()
		{
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

			weightConstant 	<<-  mixturesWeight/( sigma*sqrt( 2* pi) )

			establishTrueValues()
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
			cat("\nEvaluating Liang-Wang density example.\n\n")

			gridBase 	<- seq(-2, 12, by = 0.1)
			gridLength 	<- length( gridBase )

			grid  <- 
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
			rm( gridBase, gridLength)

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

			rm( grid, tmpRealDensityValues )
		}

####################################################################
				# Finis Structurae		
	)
)