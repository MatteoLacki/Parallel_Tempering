

PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP <- 
			function( 
				Updated_Steps_with_Last_Random_Walk,  		# boolean vector
				No_of_Chains
			)
{
	All_Chains 		<- 1:No_of_Chains
	Maximal_Lexic_No	<- No_of_Chains	* (	No_of_Chains -1   )/2
	
	return(
		setdiff(
			1:Maximal_Lexic_No,
			TRANSLATE_INTO_LEXICAL_ORDER_MANY_PAIRS_OF_INDECES(
				MATRIX_OF_PAIRS_GENERATED_BY_GIVEN_INDECES(  	
					All_Chains[!Updated_Steps_with_Last_Random_Walk]
				),
		 		No_of_Chains
			)
		)	
	)
}

	# as.matrix jako potencjalne panaceum.
###############################################################################


UPDATING_UNNORMALISED_PROBABILITIES_OF_PAIR_SWAPS <- 
			function(
				You_Wanna_Swap_i_with_j,		# boolean. If yes, then the first 
							# pair in the next variable is the one.
				Pairs_in_Lexical_Order_Needing_Update_of_Unnormalised_Probability_of_Swap,
				Current_States,
				From_Lexic_Matrix,
				Current_States_Log_Densities,
				Inverse_Temperatures,
				STRATEGY,
				QUASI_METRIC
			)
{
	

	return(
		sapply(
			Pairs_in_Lexical_Order_Needing_Update_of_Unnormalised_Probability_of_Swap,
	
				# Here we reevaluate probabilities.	
			
			function( Pair_of_Indeces_In_Lexical_Order ) 	
			{
				Pair_of_Indeces <- From_Lexic_Matrix[, Pair_of_Indeces_In_Lexical_Order]
				
				if( You_Wanna_Swap_i_with_j ) 	
				{
						# Swapped pair is the first one.
						# Observe that i_and_j is not a local variable
						# in the if statement.
						
					i_and_j <- From_Lexic_Matrix[,1]				
					if( Pair_of_Indeces[1] == i_and_j[1] ) 
					{
						Pair_of_Indeces[1] <- i_and_j[2]
					}
					
					if( Pair_of_Indeces[2] == i_and_j[2] )
					{
						Pair_of_Indeces[2] <- i_and_j[1]
					}
				}
					
				STRATEGY(
					Current_States_Log_Densities[ Pair_of_Indeces ],
					Inverse_Temperatures[ Pair_of_Indeces ], 		
					Current_States[, Pair_of_Indeces ], 
					QUASI_METRIC 	
				)
			}	  
		)
	)
}

###############################################################################

UPDATING_LOGS_OF_UNNORMALISED_DENSITIES_IN_CURRENT_STATES 	<-
			function( 
				Current_States, 
				TARGET_DENSITY 
			)
{			
	return(	
		log(
			apply( 
				Current_States, 
				2, 
				TARGET_DENSITY 
			)
		)
	)		
}

