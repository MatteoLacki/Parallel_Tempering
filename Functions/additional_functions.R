
TRANSLATE_INTO_LEXICAL_ORDER_ONE_PAIR_OF_INDECES	<- 
			function( 
				Pair_of_Indeces, 
				No_of_Chains
			) 
{
	i 	<- Pair_of_Indeces[1]
	j	<- Pair_of_Indeces[2]
	return(
		(i-1)*(No_of_Chains-i/2) + j - i
	)
}
	

TRANSLATE_INTO_LEXICAL_ORDER_MANY_PAIRS_OF_INDECES	<- 
			function( 
				Pairs_of_Indeces,	# matrix of 2 x No_of_pairs
				No_of_Chains
			)
{
	if ( class(Pairs_of_Indeces) == "NULL")
	{
		result <- c()
	}	
	else
	{
		result <-
			apply(
				Pairs_of_Indeces,	
				2,
				function( Pair_of_Indeces )
				{
					TRANSLATE_INTO_LEXICAL_ORDER_ONE_PAIR_OF_INDECES( 
											Pair_of_Indeces, 
											No_of_Chains 
					)
				}
			)		
	}
	
	return( result ) 
}

####################################################
	# Rendered better.

MATRIX_OF_PAIRS_GENERATED_BY_GIVEN_INDECES 	<- 
			function(	
				Input_Indices	
			)
{
	result <- c();
	No_of_Indices 	<- length(Input_Indices)
	

	if ( No_of_Indices > 0)
	{
		if (No_of_Indices > 1)
		{
			for ( i in 1:(No_of_Indices-1) )
				 result <- 
					cbind(
						result,
						matrix( 
							c( 
								rep.int(
									Input_Indices[ i ], 
									times = No_of_Indices-i
								), 
								Input_Indices[ (i+1):No_of_Indices ]
							),
							ncol=No_of_Indices-i, 
							nrow=2,
							byrow= TRUE
						) 
					)
		}
		else
		{
			
		}			
	}
	
	return(	result )
}




####################################################


CRUCIFIX 	<- function( Pair_of_Drawn_Indices,  No_of_Chains )

{
	i 	<- Pair_of_Drawn_Indices[1]
	j 	<- Pair_of_Drawn_Indices[2]
	
	if (i == No_of_Chains || j == 1 || i >= j) stop("I can't get no Satisfaction.", call. = FALSE)
	
	result 	<-
		cbind(
			Pair_of_Drawn_Indices,
			
			matrix( 
				c(	
					rep.int(i, times = No_of_Chains - i - 1),
					setdiff(  (i+1):No_of_Chains,  j )
				),		
				ncol	= No_of_Chains - i - 1,
				nrow 	= 2,		
			 	byrow 	= TRUE
			 ),
			 
			 matrix(
			 	c(
			 		setdiff(  1:(j-1), i),
			 		rep.int(  j, times = j - 1 - 1)	
			 	),
			 	
			 	ncol 	= j - 2,
			 	nrow	= 2,
			 	byrow	= TRUE
			 )																																																																																																																																																																																																																																																																																																																																																																																																																																																										
		)
	
	colnames(result)	<- c()
	
	return( result )
}



