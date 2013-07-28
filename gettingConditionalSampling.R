simulationsNo 	<- 10000
mixturesNo 	<- 20
sigma 		<- .1

mixturesMeans 	<- LiangWangExample$targetMeasure$mixturesMeans
measure 	<- LiangWangExample$targetMeasure$measure
contourData 	<- LiangWangExample$targetMeasure$realDensityValues 


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

X <- t(X)
require( ggplot2 )

plot(X[,1], X[,2], xlab='x', ylab='y')


png("goodSimulation.png")
	plot(X[,1], X[,2], xlab='x', ylab='y')
dev.off()

qplot(
	#data 	= dataForPlot[dataForPlot$Temperature==1,],
	data 	= X,
	aes( x = X[,1], y=X[,2])	
	#colour 	= Progress
)

 + 
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
ggtitle( "Parallel Tempering - Base Temperature" ) +
labs( x="", y="" )
