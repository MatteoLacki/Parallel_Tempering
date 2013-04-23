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
