source("./Functions/simulation_mechanism.R")
source("./Functions/ploting.R")
source("./Distributions_to_check/Liang_Example.R")

LIANNG_SIMULATION_PARALLEL_TEMPERING <- function( Steps, Details )
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
			Details		
		)
	)	
}

#system.time(Simulation_Parallel_Tempering( 1000, FALSE))
	# 65 sec!

	# The bug found and destroyed.
#PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP(	c(FALSE,FALSE,FALSE,FALSE,FALSE), 5)

#PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP(	c(FALSE,FALSE,FALSE,FALSE,TRUE), 5)

Parallel_1 <- Simulation_Parallel_Tempering( 1000, FALSE)

x <- PREPARE_DATA_FOR_2D_GGPLOT_CONTOUR( 
				Parallel_1, 
				Liang_No_of_Chains, 
				Liang_Problem_Dimension 
				)

dim(x)

tmp <- PREPARE_FULL_DATA_FOR_2D_GGPLOT_CONTOUR(x,5,1000,2,Liang_Temperatures)

tmp 		<- as.data.frame(y)
names(tmp) 	<- c("x", "y", "Temperature" ,"Progress")


tmp$Temperature	<- 	factor(
				y$Temperature,
				levels 	= Liang_Temperatures,
				ordered = TRUE
			)
Liang_Tempered_Real_Values_for_ggplot2[[1]]
,  alpha = 1/100

p <- 	qplot(x, y, data = tmp, colour=Temperature) + 
	geom_point() +
	scale_colour_brewer(type="seq", palette=3) +
	stat_contour(data=Liang_Tempered_Real_Values_for_ggplot2[[1]], aes(x, y, z =z ), bins=10, size=.5, colour="yellow") +
	ggtitle( "Parallel Tempering" ) +
	labs(x = "", y = "")



	scale_colour_gradient(limits=c(0, 1), low="green", high="black") +

LIANG_PARALLEL_TEMPERING_PLOT( 100 )

LIANG_PARALLEL_TEMPERING_PLOT( 2000 )
