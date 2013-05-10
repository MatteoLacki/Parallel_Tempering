source("./Functions/original_Metropolis_Hasting.R")
source("./Distributions_to_check/Liang_Example.R")

MH <- function( Steps , Initial_Point)
{
	mh 	<-
		MH_SIMULATION(
			No_of_Steps		= Steps,	
			Problem_Dimension 	= Liang_Problem_Dimension,
			Initial_Point		= Initial_Point,
			TARGET_DENSITY		= LIANG_TARGET_DENSITY,
			Proposal_Covariance     = Liang_Proposals_Covariance_Choleskised_Enlisted[[1]]
		)
	return( mh )
}

Steps <-1000

mh <- MH(Steps, Liang_Initial_Points[,1])
mh <- as.data.frame(mh)
mh <- cbind(mh, 1:(Steps+1)/(Steps+1))


names(mh)	<- c("x","y", "Progress")

#p <- ggplot(data = mh, aes(x=x, y=y), colour=Iteration)
p <- qplot(x, y, data = mh, colour=Progress)

Original_contour_data <- Liang_Tempered_Real_Values_for_ggplot2[[1]]

p + 	stat_contour(data=Original_contour_data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") +
	geom_point() + 
	scale_colour_gradient(limits=c(0, 1), low="green", high="black") +
	ggtitle( "Metropolis-Hastings" ) +
	labs(x = "", y = "") 
	

Original_contour_data <- Liang_Tempered_Real_Values_for_ggplot2[[1]]

METROPOLIS_HASTINGS_PLOT <- 
			function(
				Steps,
				Original_contour_data
			)
{
	Initial_Point 	<- runif(2, min=-2, max=12)
	metropolis	<- MH(Steps, Initial_Point)
	metropolis 	<- as.data.frame(metropolis)
	metropolis	<- cbind(metropolis, 1:(Steps+1)/(Steps+1))

	names(metropolis)<- c("x","y", "Progress")

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

	return(p)
}

p <- METROPOLIS_HASTINGS_PLOT(10000, Liang_Tempered_Real_Values_for_ggplot2[[1]])
p
mh_simulation_plot

head(mh)

matrix(runif(2), nrow=2, ncol=1)
