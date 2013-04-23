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

#############################################################
	# For purposes of perspective plotting.

#Liang_Distribution_Values 	<- VALUE_ESTABLISHER(Grid, LIANG_TARGET_DENSITY)
#write.csv2(Liang_Distribution_Values, "./Data/Liang_Density_Values.csv", row.names=FALSE, col.names=FALSE)

Grid 				<- seq(-2, 12, by=.2)
Liang_Tempered_Real_Values	<- vector("list", 5)
Liang_Tempered_Real_Values[[1]] <- as.matrix( read.csv2("./Data/Liang_Density_Values.csv") )	

for (i in 2:5) 
{	
	tmp 				<- Liang_Tempered_Real_Values[[1]]^Liang_Inverse_Temperatures[i]
	Liang_Tempered_Real_Values[[i]] <- tmp/sum(tmp)
}
rm(i)

LIANG_PERSPECTIVE <- function(Theta, Phi)
{
	persp(
		x=Grid,
		y=Grid, 
		z=Liang_Tempered_Real_Values[[1]], 
		theta = Theta, 
		phi = Phi,
		xlab="x",
		ylab="y",
		zlab="z",
		main=paste("Temperature = ", Liang_Temperatures[1] , sep=" "),
		cex.main=4
	)	
}



LIANG_PERSPECTIVES <- function(Theta, Phi)
{
	layout(matrix(1:4, 2, 2, byrow = TRUE))
	
		for (i in 2:5)
		{
			persp(
				x=Grid,
				y=Grid, 
				z=Liang_Tempered_Real_Values[[i]], 
				theta = Theta, 
				phi = Phi,
				xlab="x",
				ylab="y",
				zlab="z",
				main=paste("Temperature = ", Liang_Temperatures[i] , sep=" "),
				cex.main=4
			)	
		}



	layout(matrix(c(1), 1, 1, byrow = TRUE))
}

#############################################################
	# For purposes of contour plotting.

#write.csv2(	
#		as.data.frame( OTHER_VALUE_ESTABLISHER(Grid, LIANG_TARGET_DENSITY) ), 
#		"./Data/Liang_Density_Values_For_Contour_gg2plot.csv", 
#		row.names=FALSE, 
#		col.names=FALSE
#)


Liang_Tempered_Real_Values_for_ggplot2		<- vector("list", 5)
Liang_Tempered_Real_Values_for_ggplot2[[1]] 	<- 
						as.data.frame(read.csv2("./Data/Liang_Density_Values_For_Contour_gg2plot.csv"))


for (i in 2:5)
{
	Liang_Tempered_Real_Values_for_ggplot2[[i]] <- Liang_Tempered_Real_Values_for_ggplot2[[1]]
}

for (i in 2:5) 
{	
	tmp	<- Liang_Tempered_Real_Values_for_ggplot2[[1]][,3]^Liang_Inverse_Temperatures[i]
	Liang_Tempered_Real_Values_for_ggplot2[[i]][,3] <- tmp/sum(tmp)
}
rm(i)

LIANG_CONTOUR_PLOT <- function()
{
	library(ggplot2) 	

	v1 <- ggplot(
		data = Liang_Tempered_Real_Values_for_ggplot2[[1]], 
		aes(x,y,z=z)
	) +
	stat_contour(bins =10) + 
	ggtitle( paste("Temperature = ", Liang_Temperatures[1] ,sep="") )

	plot(v1)

}

LIANG_CONTOUR_PLOTS <- function()
{
	library(ggplot2) 	

	Colors <- heat.colors(5)
	Colors <- Colors[5:1]
	
	v2 <- ggplot(
		data = Liang_Tempered_Real_Values_for_ggplot2[[2]], 
		aes(x,y,z=z)
	) +
	stat_contour(
		colour=Colors[2],
		bins =10
	) + 
	ggtitle( 
		paste("Temperature = ", Liang_Temperatures[2] ,sep="")
	) +
	labs(x = "", y = "")

	v3 <- ggplot(
		data = Liang_Tempered_Real_Values_for_ggplot2[[3]], 
		aes(x,y,z=z)
	) +
	stat_contour(
		colour=Colors[3],
		bins =10
	) + 
	ggtitle( paste("Temperature =", Liang_Temperatures[3] ,sep="") ) +
	labs(x = "", y = "")
 
	v4 <- ggplot(
		data = Liang_Tempered_Real_Values_for_ggplot2[[4]],
		aes(x,y,z=z)
	) +
	stat_contour(
		colour=Colors[4],
		bins =10
	) + 
	ggtitle( paste("Temperature = ", Liang_Temperatures[4] ,sep="") ) +
	labs(x = "", y = "")

	v5 <- ggplot(
		data = Liang_Tempered_Real_Values_for_ggplot2[[5]], 	
		aes(x,y,z=z)
	) +
	stat_contour(
		colour=Colors[5],
		bins =10
	) + 
	ggtitle( paste("Temperature = ", Liang_Temperatures[5] ,sep="") ) +
	labs(x = "", y = "")


	MULTIPLOT(v2, v4, v3, v5, cols=2)	

}








