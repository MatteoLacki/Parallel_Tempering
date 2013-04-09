Liang_No_of_Clusters	<- 	20;

Liang_Clusters_Weights	<- 	rep(	1/Liang_No_of_Clusters, Liang_No_of_Clusters )

Liang_Clusters_Sigma	<- 	.1

Liang_Problem_Dimension <- 	2


Liang_Targetted_Means_of_Mixtures <- 
	matrix(
		c(2.18, 8.67, 4.24, 8.41, 3.93, 3.25, 1.70, 4.59, 6.91, 6.87, 5.41, 2.70, 4.98, 1.14, 8.33, 4.93, 1.83, 2.26, 5.54, 1.69, 5.76, 9.59, 8.48, 1.68, 8.82, 3.47, 0.50, 5.60, 5.81, 5.40, 2.65, 7.88, 3.70, 2.39, 9.50, 1.50, 0.09, 0.31, 6.86, 8.11), 
		nrow=Liang_Problem_Dimension, 
		ncol=20, 
		byrow=TRUE
	)

Liang_Temperatures 		<- 	c(1, 2.8, 7.7, 21.6, 60)

Liang_Inverse_Temperatures	<- 	1/Liang_Temperatures

Liang_No_of_Chains		<- 	length( Liang_Temperatures )

Liang_Initial_Points		<- 	replicate(
						Liang_No_of_Chains, 
						runif(	Liang_Problem_Dimension	)
					)

		# For purposes of evaluating the density
		
Liang_Means_and_Covariances_Enlisted 	<- 	vector(	"list", Liang_No_of_Clusters	)


for(	i in 1:Liang_No_of_Clusters	) 
	Liang_Means_and_Covariances_Enlisted[[i]] <- 
		list(
			Liang_Targetted_Means_of_Mixtures[,i], 
			diag(
				Liang_Clusters_Sigma, 
				nrow=Liang_Problem_Dimension, 
				ncol=Liang_Problem_Dimension
			)
		)
rm(i)

		# For purposes of simulation 

Liang_Proposals_Covariance_Choleskised_Enlisted <- 	vector(	'list', Liang_No_of_Chains )

for (	i in 1:Liang_No_of_Chains	) 
	Liang_Proposals_Covariance_Choleskised_Enlisted[[i]] <- 
		diag(
			.25*sqrt( Liang_Temperatures[i] ),
			ncol=Liang_Problem_Dimension, 
			nrow=Liang_Problem_Dimension
		)
rm(i)

#############################################################
		# Revise this. It's surely wrong, not being vectorised.

LIANG_TARGET_DENSITY <- function(x)
{
	return(WEIGHTED_NORMAL_DISTRIBUTION(x, Liang_Clusters_Weights, Liang_Means_and_Covariances_Enlisted))
}

#system.time(LIANG_TARGET_DENSITY(1:2))
