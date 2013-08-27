############################### Loading files ################################
rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")

############################### State-dependent simulation ###################


BigSimulaton <- function( trialNo, minStrat, maxStrat )
{
  euclid <- function(x,y)
  {
  	return( crossprod(x-y) )
  }
  
  strategyNo  <- maxStrat - minStrat + 1
  
  results <- as.data.frame(matrix(nrow=trialNo*strategyNo,ncol=63))
  
  nameCreator <- function( letter, minNo, maxNo)	
  {
  	sapply(
  		minNo:maxNo,
  		function(num){
  			return( paste( letter, num, sep="",collapse="" ) )
  		}
  	)	
  }
  	
  naming  <- c(
  	'Strategy',
  	nameCreator('rwByTemp',1,5), 
  	nameCreator('rswap',0,10),
  	'KS',
  	nameCreator('MeanNo',1,20),
  	nameCreator('MeanNo',1,20),
  	'EX',
  	'EY',
  	'EX2',
  	'EY2',
  	'EXY'
  ) 	
  
  i <- 1L
  
  
    for( strategy in minStrat:maxStrat ){
    	for( trial in 1:trialNo ){
          LiangWangExample <- simulation$new(
      			iterationsNo	= 7500,
      			strategyNo 	= strategy,
      			example 	= TRUE,
      			burnIn 		= 2500,
      			save		= FALSE,
      			trialNo 	= trial,
      			quasiMetric 	= euclid,
      			evaluateKS 	= TRUE
      		)
      		LiangWangExample$simulate()
      		results[i,] <- LiangWangExample$furnishResults()
      		i <- i+1
      		rm(LiangWangExample)		 
    	}
    }
  
  
  names( results ) <- naming
  
  return( results )
}

BigSimulaton( 2, 1, 1)

############################### Additional Topics ############################
rm( list = ls())
#directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#setwd(directory)

source("./targetMeasures/targetMeasures.R")
source("./targetMeasures/targetUnnormalisedDensities.R")
source("./targetMeasures/targetLiangDensities.R")
source("./targetMeasures/targetMatteoDensities.R")
source("./functionsToIntegrate/functionsToIntegrate.R")
source("./stateSpaces/stateSpaces.R")
source("./stateSpaces/realStateSpaces.R")
source("./stateSpaces/realTemperedStateSpaces.R")
source("./algorithms/algorithms.R")
source("./algorithms/metropolisHastings.R")
source("./algorithms/parallelTemperings.R")
source("./simulations/simulations.R")
source("./controllers/controllers.R")


f <- function( x ){ return( c( x, x^2, x[1]*x[2]) )}

LiangWangExample <- simulation$new(
	iterationsNo	= 7500,
	strategyNo 	= 2,
	example 	= TRUE,
	burnIn 		= 2500,
	save		= FALSE,
	trialNo 	= 1L,
	evaluateKS 	= FALSE,
	integratedFunction = f,
	rememberStates  = FALSE,
	evaluateSojourn = TRUE
)

system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample$furnishResults()
LiangWangExample$algorithm$plotHistory()

x <- LiangWangExample$algorithm$transpositionsHistory
x
y <- x[2,]/x[1,]

cnt<- ncol(x)
y <- numeric(2*cnt)
y[1:cnt] <- x[2,]
y[(cnt+1):(2*cnt)]<- x[1,]-x[2,]
z <- as.data.frame(y)
z[1:cnt,2] <- 'Accepted'
z[(cnt+1):(2*cnt),2]<- 'Rejected'
z[1:cnt,3] <- xAxisTags
z[(cnt+1):(2*cnt),3]<- xAxisTags
names(z) <- c('Count','Acceptance','Transposition')
z$labelY[1:cnt]  <- x[2,]
z$labelY[(cnt+1):(2*cnt)]  <- x[1,]+max(x[1,])/40
z$relativeValue[1:cnt] <- round(x[2,]/x[1,],digits=2 )
z$relativeValue[(cnt+1):(2*cnt)] <- round(1 - x[2,]/x[1,], digits=2)
z

xAxisTags <- 
	apply(
		LiangWangExample$algorithm$translatorFromLexicOrderToTranspositions,
		2,
		function( transposition )
		{
			paste(
				"(", 
				transposition[1], 
				",",
				transposition[2],
				")", 
				sep="", 
				collapse=""
			)	
		}
	)

ggplot(
	z,
	aes(x = Transposition, y= Count, fill=Acceptance)
)+
geom_bar(stat="identity") +
geom_text(aes(y=labelY, label=relativeValue), vjust=1.5, colour="black", size=4)+
scale_fill_brewer(palette="Pastel1")+ 
				labs(
					title ="Swaps distribution"
				)


LiangWangExample$integrant$approximation
barplot(x[1,])
qplot(
	1:length(y),
	y,
	geom="bar",
	stat="identity"
)



svg("histogram.svg", width=6, height=4)
	LiangWangExample$algorithm$plotHistory()
dev.off()

############################### New Measure ############################
source("./targetMeasures/targetMatteoDensities.R")
LiangWangExample <- simulation$new(
	iterationsNo	= 10000,
	strategyNo 	= 1,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$stateSpace$initializeEcdfData()
Z <- LiangWangExample$stateSpace$ecdfData
#length(Z$x)
anyDuplicated(c(Z$x,Z$y))
anyDuplicated(Z$x)
anyDuplicated(Z$y)
# For 36000 different points with calculated charges: no difference whatsoever. So why to waste money ?

################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 2,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

svg("MatteoDistributionStrategy2.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()




################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 6,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

svg("MatteoDistributionStrategyUniformDistributionOnNeighbouringTranspositions.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()


################################################
LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 5,
	example 	= TRUE,
	targetMeasureName = 'Matteo'	
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	
LiangWangExample$stateSpace$initializeEcdfData()
system.time(
  LiangWangExample$stateSpace$kolmogorovSmirnov(resolution=0)
)
KS <- LiangWangExample$stateSpace$KS
KS

svg("MatteoDistributionStrategyUniformDistributionOnAllTranspositions.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()

################################################


kwaziNazi <- function(x,y)
{
	return( crossprod(x-y) )
}

LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 4,
	example 	= TRUE,
	targetMeasureName = 'Matteo',
	quasiMetric	= kwaziNazi
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	


svg("MatteoDistributionStrategyWithQuasiMetric.svg", width=12, height=8)
	LiangWangExample$stateSpace$plotBaseTemperature()	
dev.off()


svg("MatteoDistributionStrategyWithQuasiMetricTranspositionHistory.svg", width=12, height=8)
	LiangWangExample$algorithm$plotHistory()
dev.off()

################################################

tmpProposalCovariances 	<- vector( "list", 5L )

for (i in 1:5 )
{
	tmpProposalCovariances[[i]] <- 
		diag( 
			ifelse(i <=3, .05, .01)*1,
			nrow=2, 
			ncol=2 
		) 				
}

LiangWangExample <- simulation$new(
	iterationsNo	= 1000,
	strategyNo 	= 6,
	algo 	 	= 'Metropolis-Hastings',
	space 	 	= 'real',
	target 		= 'Liang-Wang',
	covariances	= tmpProposalCovariances,
	spaceDim	= 2,
	chainsNo 	= 5	 
)

LiangWangExample <- Metro(
	n		= 1000,
	strategyNo 	= 6,
	space 	 	= 'real',
	target 		= 'Liang-Wang',
	covariances	= tmpProposalCovariances,
	spaceDim	= 2,
	chainsNo 	= 5	 
)

LiangWangExample <- PT(
	n		= 1000,
	strategyNo 	= 6,
	example	 	= TRUE
)

LiangWangExample
system.time(
  LiangWangExample$simulate()  
) 
LiangWangExample	

