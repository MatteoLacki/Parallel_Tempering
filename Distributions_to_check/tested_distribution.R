# General Gaussian mixed distribution

# ok
WEIGHTED_NORMAL_DISTRIBUTION <- 
		function(
			x, 
			Weights, 
			Means_and_Covariances_Enlisted)
{ 
	return(sum(Weights * sapply( Means_and_Covariances_Enlisted, function(y) dmvnorm(x, mean=y[[1]], sigma=y[[2]]) )))
}


#############################################################
# Revise this. It's surely wrong, not being vectorised.

VALUE_ESTABLISHER <- function( 	grid, 	FUNCTION_INPUT	)
{
	tmp <- length(grid)
	Z 	<- matrix(nrow = tmp, ncol = tmp)
	
	for( i in 1:tmp)
	{
		for(j in 1:tmp)
		{
			Z[i,j] <- FUNCTION_INPUT(c(grid[i], grid[j]))
		}	
	}	
	
	return(Z)		
}

BETTER_VALUE_ESTABLISHER <- function()
{
		
}

