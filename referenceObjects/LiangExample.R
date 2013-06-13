
LiangNoOfClusters	<- 	20;
LiangClustersWeights<- 	rep( 1/LiangNoOfClusters, LiangNoOfClusters )
LiangClusterSigma	<- 	.1
LiangSpaceDimension	<- 2

LiangMeansOfMixtures<- 
	matrix(
		c(2.18, 8.67, 4.24, 8.41, 3.93, 3.25, 1.70, 4.59, 6.91, 6.87, 5.41, 2.70, 4.98, 1.14, 8.33, 4.93, 1.83, 2.26, 5.54, 1.69, 5.76, 9.59, 8.48, 1.68, 8.82, 3.47, 0.50, 5.60, 5.81, 5.40, 2.65, 7.88, 3.70, 2.39, 9.50, 1.50, 0.09, 0.31, 6.86, 8.11), 
		nrow=LiangSpaceDimension, 
		ncol=20, 
		byrow=TRUE
	)

LiangTemperatures 		<- c(1, 2.8, 7.7, 21.6, 60)
LiangInverseTemperatures<- 1/LiangTemperatures
LiangNoOfTemperatures	<- 	length( LiangTemperatures )
LiangInitialPoints		<- 	
	replicate(
		LiangNoOfTemperatures, 
		runif(	LiangSpaceDimension	)
	)

	# For purposes of evaluating the density
		
LiangMeansAndCovariances <-	vector(	"list", LiangNoOfClusters	)

for( i in 1:LiangNoOfClusters ) 
	LiangMeansAndCovariances[[i]] <- 
		list(
			LiangMeansOfMixtures[,i], 
			diag(
				LiangClusterSigma, 
				nrow=LiangSpaceDimension, 
				ncol=LiangSpaceDimension
			)
		)

rm(i)

LiangProposalCovariancesCholeskised <- vector(	'list', LiangNoOfTemperatures )

for( i in 1:LiangNoOfTemperatures ) 
	LiangProposalCovariancesCholeskised[[i]] <- 
		diag(
			.25*sqrt( LiangTemperatures[i] ),
			ncol=LiangSpaceDimension, 
			nrow=LiangSpaceDimension
		)
rm(i)

#############################################################
		# Revise this. It's surely wrong, not being vectorised.
WEIGHTED_NORMAL_DISTRIBUTION <- 
		function(
			x, 
			Weights, 
			Means_and_Covariances_Enlisted)
{ 
	return(
		sum(
			Weights * sapply(
					Means_and_Covariances_Enlisted, 
					function(y) dmvnorm(x, mean=y[[1]], sigma=y[[2]]) 
			)
		)
	)
}


LIANG_TARGET_DENSITY <- function(x)
{
	return(WEIGHTED_NORMAL_DISTRIBUTION(x, LiangClustersWeights, LiangMeansAndCovariances))
}




LiangWangExampleTest1 <- ParallelTempering$new(
	noOfIterations	= 1000,
	temperatures 	= c(2.8, 7.7, 21.6, 60),	
	strategyNumber  = 2,
	problemDimension= 2,
	targetDensity	= LIANG_TARGET_DENSITY,
	detailedOutput	= FALSE
)


