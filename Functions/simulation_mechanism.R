source("./Functions/additional_functions_for_simulation.R")


SIMULATION <- 		function(
				No_of_Chains,
				No_of_Steps, 
				Problem_Dimension,
				Initial_Points,
				TARGET_DENSITY,
				STRATEGY, 
				QUASI_METRIC,
				Proposals_Covariance_Choleskised_Enlisted,
				Inverse_Temperatures,
				Show_Details = FALSE #boolean - details on screen.	
				)
{
			# Preparing the dictionary from lexical ordering of
			# the upper triangle of the matrix to indices.
			 
	From_Lexic_Matrix <- MATRIX_OF_PAIRS_GENERATED_BY_GIVEN_INDECES( 1:No_of_Chains )

	Chains 	 	<- vector(	"list", 	2 * No_of_Steps	+ 1 )
	Chains[[1]]	<- Initial_Points
	
		
		# That's copy-paste really - need a function that does precisely that.
		# Because it's also done in the PROPOSED_STEP function. The code will 
		# get nicer.
		
	Log_Densities_of_Current_States	<- 
			UPDATING_LOGS_OF_UNNORMALISED_DENSITIES_IN_CURRENT_STATES(
				Initial_Points,
				TARGET_DENSITY
			)	
			
#			log(
#				apply( 
#					Initial_Points, 
#					2, 
#					TARGET_DENSITY 
#				)
#			)


	
			# The maximal number in lexicographic ordering of 
			# the upper-triangular matrix of indices.
		 
	Maximal_Lexic_No	<- No_of_Chains	* (	No_of_Chains -1   )/2
	
	if (Show_Details){ print("Hello Simulation") }		
	
			# This is needed for the first go of the Swap Kernel.	
	
	Current_Unnormalised_Probabilities_of_Pair_Swaps<- 					
			UPDATING_UNNORMALISED_PROBABILITIES_OF_PAIR_SWAPS(
				FALSE,
				1:Maximal_Lexic_No,
				Initial_Points,
				From_Lexic_Matrix,
				Log_Densities_of_Current_States,
				Inverse_Temperatures,
				STRATEGY,
				QUASI_METRIC
			)

	
	
	for( i in (2* 1:No_of_Steps ))
	{

		if (Show_Details){ 
			print("===============================================================")
			print(c("Random Walk No ", i/2))
		}
	
		New_Step_and_New_Log_Densities <- 
				CHAIN_STEP(
						Chains[[i-1]], 
						TARGET_DENSITY, 
						No_of_Chains, 
						Problem_Dimension, 
						Log_Densities_of_Current_States,
						Proposals_Covariance_Choleskised_Enlisted,
						Inverse_Temperatures,
						Show_Details
				) 
		
			
		Chains[[i]] 			<- New_Step_and_New_Log_Densities[[1]]
		Log_Densities_of_Current_States	<- New_Step_and_New_Log_Densities[[2]]

		if (Show_Details){		
			print("Steps after Random Walk")
			print(Chains[[i]])
		}			
				# This is not needed as input with the first step.

		Updated_Steps_in_Random_Walk	<- New_Step_and_New_Log_Densities[[3]] 	# Boolean vector
								
		Swapping_Current_States		<- 
				SWAP_STEP(
						No_of_Chains, 
						Maximal_Lexic_No,
						Chains[[i]],
						From_Lexic_Matrix,
						Log_Densities_of_Current_States,
						Current_Unnormalised_Probabilities_of_Pair_Swaps,
						Updated_Steps_in_Random_Walk,
						Inverse_Temperatures, 
						STRATEGY,
						QUASI_METRIC,
						Show_Details
				)
						
		Permutation_of_Swap_Step			<- Swapping_Current_States[[1]]
		Current_Unnormalised_Probabilities_of_Pair_Swaps<- Swapping_Current_States[[2]]	
			
		if (Show_Details){ 
			print(c("Random Swap No ", i/2))
			print( Permutation_of_Swap_Step )	
		}	
	
			
				# Here we apply the swap to the chains and
				# to the log_densities. 
				# Only the Unnormalised probs of pair swaps need no permutation update. 
			
		Chains[[i+1]] 	<- Chains[[i]][, Permutation_of_Swap_Step ]
		Log_Densities_of_Current_States <- Log_Densities_of_Current_States[ Permutation_of_Swap_Step ]
	
		if (Show_Details){ 
			print("Steps after Random Swap")
			print(Chains[[i+1]])
		}
			
	}
	
	return(Chains)
}

###############################################################################



CHAIN_STEP	<- 	function(
				Current_States, 				#matrix
				TARGET_DENSITY,					#function 
				No_of_Chains, 					#int
				Problem_Dimension, 				#int
				Previous_Log_Densities,				#vector
				Proposals_Covariance_Choleskised_Enlisted, 	#ListOfMatrices
				Inverse_Temperatures, 				#vector
				Show_Details					#boolean
				)
{
	Proposal_Step_for_all_chains	<- Current_States + 
					sapply(
						Proposals_Covariance_Choleskised_Enlisted, 
						function(x) {x%*% rnorm(Problem_Dimension)}
					)
	
	if (Show_Details){ print("Hello CHAIN_STEP") }
		
			# Logs of uniform distribution
			
	Ulog <- log( runif(No_of_Chains) )
	
			# Calculate a vector with log pi(proposal_i) 
			
	Proposal_Log_Densities <- 
			UPDATING_LOGS_OF_UNNORMALISED_DENSITIES_IN_CURRENT_STATES(
				Proposal_Step_for_all_chains,
				TARGET_DENSITY
			)	
				
#				log(
#					apply( 
#						Proposal_Step_for_all_chains, 
#						2, 
#						TARGET_DENSITY 
#					)
#				)

	if (Show_Details){ 
		print("Previous Log Densities")
		print(Previous_Log_Densities)
		print("Proposal Log Densities")
		print(Proposal_Log_Densities)	
	}

	
	Quantities_to_be_Compared_with_Log_Uniform_RV <-
	apply(
		rbind( Inverse_Temperatures, Proposal_Log_Densities, Previous_Log_Densities ),
		2,
		function( triple ) triple[1]*(triple[2] - triple[3])
	)

	if (Show_Details){ 	
		print("Quantities to be Compared with Log Uniform RV")
		print(Quantities_to_be_Compared_with_Log_Uniform_RV)
		print("Ulog")
		print(Ulog)	
	}

		# It's Ulog < ... because then we must accept the proposals. 
		
	Updated_Steps <- Ulog < Quantities_to_be_Compared_with_Log_Uniform_RV 

	if (Show_Details){ 
		print("Updated Steps")
		print(Updated_Steps)
	}
		### ERROR: Erreur dans 
		### Proposal_Step_for_all_chains[, Updated_Steps ] <- 
		### Current_States[, Updated_Steps ]	
		### NAs interdits dans les affectatoins indicees.
	
	
		# Updating the states that were accepted.
	
	Current_States[, Updated_Steps ]	<- Proposal_Step_for_all_chains[, Updated_Steps ]
	
		# Here we update the log_densities. The old ones get lost: ain't that a pity?
		
	New_Log_Densities			<- 	Previous_Log_Densities
	New_Log_Densities[Updated_Steps] 	<- 	Proposal_Log_Densities[Updated_Steps]
	


		# Preparing results
		
	Results_Enlisted 			<- 	vector("list", 3)
		
	Results_Enlisted[[1]] 			<- 	Current_States
	Results_Enlisted[[2]]			<-	New_Log_Densities  
	Results_Enlisted[[3]]			<- 	Updated_Steps	
				
	
 	return(	Results_Enlisted )
}




###############################################################################
		# Will return a permutation of columns of the Current_States Matrix
		# hidden under the appropriate Chain[[i]].
		
SWAP_STEP 	<- 	function(
				No_of_Chains,					#int
				Maximal_Lexic_No,				#int
				Current_States, 				#matrix
				From_Lexic_Matrix, 				#matrix
				Current_States_Log_Densities, 			#vector
				Current_Unnormalised_Probabilities_of_Pair_Swaps,#vector
				Updated_Steps_with_Last_Random_Walk,		#boolean vector.
				Inverse_Temperatures, 				#vector
				STRATEGY, 					#function
				QUASI_METRIC,					#function
				Show_Details					#boolean
				)
{
		# Preparing the pairs of indeces that have to be updated.
	
	Pairs_in_Lexical_Order_Needing_Update_of_Unnormalised_Probability_of_Swap <-
			 
			 PAIRS_IN_LEXICAL_ORDER_NEEDING_UPDATE_OF_UNNORMALISED_PROBABILITY_OF_SWAP(
			 	Updated_Steps_with_Last_Random_Walk,
			 	No_of_Chains	
			 )
	
	
		# Here we calculate swap index probabilities in places where something 
		# did change in the last Random Walk

	Current_Unnormalised_Probabilities_of_Pair_Swaps[
		Pairs_in_Lexical_Order_Needing_Update_of_Unnormalised_Probability_of_Swap
			] <- 
			UPDATING_UNNORMALISED_PROBABILITIES_OF_PAIR_SWAPS(
				FALSE,
				Pairs_in_Lexical_Order_Needing_Update_of_Unnormalised_Probability_of_Swap,
				Current_States,
				From_Lexic_Matrix,
				Current_States_Log_Densities,
				Inverse_Temperatures,
				STRATEGY,
				QUASI_METRIC
			)

				
		# Drawing a random pair of indeces (i,j) = m from given strategy
	
	Proposed_Pair_for_Swap_in_Lexical_Order	<- 	
			sample(
				1:Maximal_Lexic_No,  
				size = 1,
				prob = Current_Unnormalised_Probabilities_of_Pair_Swaps
			)	

	Proposal_Swap	<-	From_Lexic_Matrix[, Proposed_Pair_for_Swap_in_Lexical_Order ]
		
		# Having drawn (i,j) we must now prepare additional info on statistical sum of
		# p_{ij}( S_{ij} x), where x is the current states matrix.
		# CRUCIFIX returns pair of indeces	
	
	Cross 		<- 	
			CRUCIFIX(
				Proposal_Swap,
				No_of_Chains
			)
		
#	Cross_Lexic 	<-	
#			apply(
#				Cross,
#				2,
#				function(x) 
#				{
#					TRANSLATE_INTO_LEXICAL_ORDER(
#						x,
#						No_of_Chains
#					)
#				}	
#			)	

	Cross_Lexic	<- 	TRANSLATE_INTO_LEXICAL_ORDER_MANY_PAIRS_OF_INDECES(
					Cross,
					No_of_Chains
				)	
		
 		# First entry in Additional.. is STRATEGY_ij (S_ij x). It's there because of TRUE tag.
 		# Here we calculate the additional terms that must appear in the 
 		# statistical sum of p_{ij}( S_{ij} x).
 		

	Additional_Unnormalised_Probabilities_of_Swaping_Indeces <-
			UPDATING_UNNORMALISED_PROBABILITIES_OF_PAIR_SWAPS(
				TRUE,
				Cross_Lexic,
				Current_States,
				From_Lexic_Matrix,
				Current_States_Log_Densities,
				Inverse_Temperatures,
				STRATEGY,
				QUASI_METRIC
			)


	# Preparing inputs for the acceptance rule. 
			
		# These are unnormalised switch probabilities
		# So they are updated on the (k,l) that were changed due to drawing (i,j) previously.
		
	Proposal_Swap_Index_Probabilities			<- 
						Current_Unnormalised_Probabilities_of_Pair_Swaps
	Proposal_Swap_Index_Probabilities[ Cross_Lexic ] 	<- 
						Additional_Unnormalised_Probabilities_of_Swaping_Indeces

		
		# This is in the nominator of the normalised probability after switch.
		
	Unnormalised_Probability_of_Swap_of_the_Drawn_Pair_of_Indeces <- 					
						Additional_Unnormalised_Probabilities_of_Swaping_Indeces[1]
				
				
		# The after-minus-part is the statistical sum of p_{ij}(S_{ij} x)
		# needed for the acceptance rule.
				
	Swap_Proposal_Log_Density <- 	
			log(	Unnormalised_Probability_of_Swap_of_the_Drawn_Pair_of_Indeces	) -
	 		log(		sum( Proposal_Swap_Index_Probabilities )		)



		# Current_Unnormalised_Probabilities_of_Pair_Swaps[Proposed_Pair_for_Swap_in_Lexical_Order] is the nominator of the 
		# Probability p_{ij} (x).

	Swap_Current_Log_Density <- 
	
	log( Current_Unnormalised_Probabilities_of_Pair_Swaps[Proposed_Pair_for_Swap_in_Lexical_Order] 	) - 
	log(		sum( Current_Unnormalised_Probabilities_of_Pair_Swaps )				)




		
		# The acceptance rule in action. Proposal_Swap is the drawn pair of indeces.

		
	Proposal_Inverse_Temperatures	<- 	Inverse_Temperatures[ Proposal_Swap ] 
	Target_Log_Densities		<-	Current_States_Log_Densities[ Proposal_Swap ] 

	
	Log_Alpha			<- 	( 
							Proposal_Inverse_Temperatures[1] 
							- 	
							Proposal_Inverse_Temperatures[2] 
						) * 
						(
							Target_Log_Densities[2] 
							-
							Target_Log_Densities[1]
						) +
						Swap_Proposal_Log_Density -
						Swap_Current_Log_Density
	
	Log_U 				<- log(	runif(1) )
		
		
		# Response is a neutral permutation...
		
	
	
		# ... and so it might get updated.

	Swap_Step_Results		<- vector("list", 2)
	Response_Permutation		<- 1:No_of_Chains	
	Swap_Step_Results[[2]]		<- Current_Unnormalised_Probabilities_of_Pair_Swaps	

		
		# If the proposal gets accepted...
		
	#	print(c(Log_U, Log_Alpha))	# Seems it's ok
	if ( Log_U < Log_Alpha )  	{
						
						# ... take the right permutation
						
					Response_Permutation[Proposal_Swap] <- 
							c(
								Proposal_Swap[2],
								Proposal_Swap[1]
							)	
							
						# ... and write down the correct probabilities. 
											
					Swap_Step_Results[[2]] 	<- Proposal_Swap_Index_Probabilities
													
					} 
		
	#	print(Response_Permutation)	# Seems it's ok.
	
	Swap_Step_Results[[1]]		<- Response_Permutation
	
		
		# The brand new unnormalised probabilities of drawing a pair of indeces for a swap.	

		
	return( Swap_Step_Results )
}

###############################################################################
