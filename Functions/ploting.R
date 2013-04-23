library( ggplot2 )

####################################################
	# Foreign function for multi-ggploting 

MULTIPLOT 	<- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


####################################################
	# Foreign function for multi-ggploting 


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

####################################################

	
PREPARE_DATA_FOR_2D_GGPLOT_CONTOUR	 <- 
			function( 	
				List_of_Generated_Chains, 
				No_of_Chains, 
				Problem_Dimension
				)
{
	if( Problem_Dimension != 2) stop("Wrong dimension.")
	
	matrix_laying <- sapply(	
			List_of_Generated_Chains, 
			function( Generated_Step_Results ){ 
				tmp 				<- numeric(10)
				tmp[(2*0:(No_of_Chains-1) + 1)] <- Generated_Step_Results[ 1, 1:No_of_Chains]
				tmp[ 2*1:No_of_Chains]		<- Generated_Step_Results[ 2, 1:No_of_Chains]
				return(tmp)		
				} 
		)
	
	return( t( matrix_laying ) )	
	
}


####################################################
	# This function heavily depends on the strategy of random walk and random swap.

PREPARE_FULL_DATA_FOR_2D_GGPLOT_CONTOUR	 <- 
			function(
				Matrix_of_Chains, 
				No_of_Chains, 
				No_of_Steps,
				Problem_Dimension, 
				Temperatures
				)
{
				# We include only the random walk steps.
	Matrix_of_Chains <- Matrix_of_Chains[c(1, 2*(1:No_of_Steps) ), ]		

	X <- matrix(ncol = 4, nrow= No_of_Chains*(No_of_Steps + 1) )

	for (i in 1:No_of_Chains) 
	{
		X[(i + (i-1)*No_of_Steps):( i*(No_of_Steps +1 )  ),1:2] <- Matrix_of_Chains[,(2*i-1):(2*i)]
		X[(i + (i-1)*No_of_Steps):( i*(No_of_Steps +1 )  ),3] 	<- rep.int( 
										Temperatures[i], 
										times=( No_of_Steps + 1) 
										)
		X[(i + (i-1)*No_of_Steps):( i*(No_of_Steps +1 )  ),4] 	<- 1:(No_of_Steps + 1)/(No_of_Steps+1)	
	}
	
	
	return(X)	
}
