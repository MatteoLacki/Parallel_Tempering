# The unnormalised probobility functions.

# Two_Current_States_Log_Densities[2] is log(pi(x_j))
# Two_Current_States_Log_Densities[1] is log(pi(x_i))


STRATEGY_ONE <- 
	function(
		Two_Current_States_Log_Densities, 
		...)
{
	return(
		exp(
			-abs( 
			Two_Current_States_Log_Densities[1] - Two_Current_States_Log_Densities[2] 
			) 
		)    
	    
	)
}

STRATEGY_TWO <- 
	function(
		Two_Current_States_Log_Densities, 
		...)
{
	return(
		min(	
			1,
			exp( 
				Two_Current_States_Log_Densities[2] - Two_Current_States_Log_Densities[1]  
			)
		)
	)
}


STRATEGY_THREE <- 
	function(
		Two_Current_States_Log_Densities,
		Two_Inverse_Temperatures, 
		...)
{
	return(
	    exp( 
	    	-( Two_Inverse_Temperatures[1] - Two_Inverse_Temperatures[2] ) * 
	    	abs( Two_Current_States_Log_Densities[1] - Two_Current_States_Log_Densities[2] ) 
	    	)    
	)

}

STRATEGY_FOUR <- 
	function(
		Two_Current_States_Log_Densities, 
		Two_Inverse_Temperatures, 
		Two_States_Matrix, 
		QUASI_METRIC)
{
	return(    
		exp( 
			-( Two_Inverse_Temperatures[1] - Two_Inverse_Temperatures[2] ) 
			*
			abs( Two_Current_States_Log_Densities[1] - Two_Current_States_Log_Densities[2] )
			/
			( 1 + QUASI_METRIC(
				Two_States_Matrix[,1],
				Two_States_Matrix[,2]
				)
			) 
		)    		
	)

}



EASY_METRIC 	<- function(x, y)
{
	return( 
		sum((x-y)^2) 
	)
}
