rm( list = ls())
d
irectory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)

source("./Distributions_to_check/tested_distribution.R")
source("./Functions/simulation_mechanism.R")
source("./Functions/additional_functions.R")
source("./Strategies_to_check/tested_strategies.R")
source("./Functions/ploting.R")
source("./Distributions_to_check/Liang_Example.R")

LIANG_SIMULATION_PARALLEL_TEMPERING <- function( Steps, Details=FALSE )
{
	return(
		SIMULATION(
			Liang_No_of_Chains,
			Steps,	
			Liang_Problem_Dimension,
			Liang_Initial_Points,
			LIANG_TARGET_DENSITY,
			STRATEGY_FOUR,
			EASY_METRIC,
			Liang_Proposals_Covariance_Choleskised_Enlisted,		
			Liang_Inverse_Temperatures,
			Show_Details = Details		
		)
	)	
}

LIANG_PARALLEL_TEMPERING_PREPARING_DATA <- function( No_of_Steps, Details )
{

	Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( No_of_Steps, FALSE)		
	
	Parallel <- PREPARE_DATA_FOR_2D_GGPLOT_CONTOUR( 
				Parallel, 
				Liang_No_of_Chains, 
				Liang_Problem_Dimension 
				)

	Parallel <- PREPARE_FULL_DATA_FOR_2D_GGPLOT_CONTOUR(
				Parallel,
				Liang_No_of_Chains,
				No_of_Steps,
				Liang_Problem_Dimension ,
				Liang_Temperatures
				)

	return(	Parallel )	
}

source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

LiangWangExample <- ParallelTempering$new(
	noOfIterations	= 100,
	temperatures 	= c(2.8, 7.7, 21.6, 60),	
	strategyNumber  = 2,
	problemDimension= 2,
	targetDensity	= LIANG_TARGET_DENSITY,
	detailedOutput	= FALSE,
	proposalCovariances = list(matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(100,0,0,2), ncol=2,nrow=2),matrix(c(4,0,0,2), ncol=2,nrow=2),matrix(c(6,0,0,2), ncol=2,nrow=2))
)


Rprof("hello.out")
	LiangWangExample$simulate()     
Rprof(NULL)

LiangWangExample$stateSpace$prepareData()



system.time( LIANG_SIMULATION_PARALLEL_TEMPERING( 1000 ) )
system.time( LiangWangExample$simulate() )
	# 65 sec!

	# The bug found and destroyed.
#PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP(	c(FALSE,FALSE,FALSE,FALSE,FALSE), 5)

#PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP(	c(FALSE,FALSE,FALSE,FALSE,TRUE), 5)

Parallel_1 <- Simulation_Parallel_Tempering( 100, FALSE)
Parallel_1 <- LIANG_SIMULATION_PARALLEL_TEMPERING( 100, FALSE)

x <- PREPARE_DATA_FOR_2D_GGPLOT_CONTOUR( 
				Parallel_1, 
				Liang_No_of_Chains, 
				Liang_Problem_Dimension 
				)

dim(x)

tmp <- PREPARE_FULL_DATA_FOR_2D_GGPLOT_CONTOUR(x,5,100,2,Liang_Temperatures)

dim(tmp)
head(tmp)
tmp 		<- as.data.frame(tmp)
head(tmp)
names(tmp) 	<- c("x", "y", "Temperature" ,"Progress")
head(tmp)

tmp$Temperature	<- 	factor(
				tmp$Temperature,
				levels 	= Liang_Temperatures,
				ordered = TRUE
			)
head(tmp)

tmp[tmp$Temperature==1,]


Liang_Tempered_Real_Values_for_ggplot2[[1]]
,  alpha = 1/100

p <- 	qplot(x, y, data = tmp, colour=Progress) + 
	geom_point() +
	scale_colour_gradient(limits=c(0, 1), low="white", high="black") +
	stat_contour(data=Liang_Tempered_Real_Values_for_ggplot2[[1]], aes(x, y, z =z ), bins=5, size=.5, colour="red") +
	ggtitle( "Parallel Tempering - Base Temperature" ) +
	labs(x = "", y = "")

p


	scale_colour_gradient(limits=c(0, 1), low="green", high="black") +

LIANG_PARALLEL_TEMPERING_PLOT( 100 )

LIANG_PARALLEL_TEMPERING_PLOT( 2000 )




	names(Parallel) <- c("x", "y", "Temperature" ,"Progress")

	Parallel 	<- Parallel[Parallel$Temperature==1,]
	
	p <- 	qplot(x, y, data = tmp, colour=Progress) + 
	geom_point() +
	scale_colour_gradient(limits=c(0, 1), low="white", high="black") +
	stat_contour(data=Liang_Tempered_Real_Values_for_ggplot2[[1]], aes(x, y, z =z ), bins=5, size=.5, colour="red") +
	ggtitle( "Parallel Tempering - Base Temperature" ) +
	labs(x = "", y = "")


Parallel 	<- as.data.frame(Parallel)
Parallel 	<- Parallel[Parallel$Temperature==1,]

p <- 	qplot(x, y, data = metropolis, colour=Progress) + 
		stat_contour(data=Original_contour_data, aes(x, y, z =z ), bins=10, size=.5, colour="grey50") +
		geom_point() + 
		scale_colour_gradient(limits=c(0, 1), low="green", high="black") +
		ggtitle( 
			paste(
				"Metropolis-Hastings starting at (x,y) =( ", 
				round(Initial_Point[1], digits=2 ),
				" , ",
				round(Initial_Point[2], digits=2 ),
				" )",
				sep="" 
			) 
		) +
		labs(x = "", y = "")

