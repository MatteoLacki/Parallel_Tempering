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


#############################################################
OTHER_VALUE_ESTABLISHER <- function( grid,   FUNCTION_INPUT )
{
  tmp <- length(grid)
  Z 	<- matrix(nrow=tmp^2, ncol=3)
  
  for( j in 1:tmp)
  {
    for(i in 1:tmp)
    {
      Z[i+(j-1)*tmp,1] <- grid[i]
      Z[i+(j-1)*tmp,2] <- grid[j]
      Z[i+(j-1)*tmp,3] <- FUNCTION_INPUT(c(grid[i], grid[j]))
    }	
  }	
  
  return(Z)		
}

