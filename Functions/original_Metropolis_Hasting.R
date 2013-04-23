source("./Functions/additional_functions_for_simulation.R")

MH_SIMULATION <- 	function(
				No_of_Steps, 
				Problem_Dimension,
				Initial_Point,
				TARGET_DENSITY,
				Proposal_Covariance,
				Show_Details = FALSE #boolean - details on screen.	
				)
{
	if (Show_Details){ print("Hello Simulation") }

			# The first coordinate is taken by the initial draw of points.
	Chain 	 	<- matrix(ncol=Problem_Dimension, nrow=(No_of_Steps+1) )
	Chain[1,]	<- Initial_Point
	
	if (Show_Details){ 
		print("Initial Point")
		print(Initial_Point)
	}
		
		# That's copy-paste really - need a function that does precisely that.
		# Because it's also done in the PROPOSED_STEP function. The code will 
		# get nicer.
		
	Log_Density_of_Current_State	<- 
			UPDATING_LOG_OF_UNNORMALISED_DENSITY_IN_CURRENT_STATE(
				Initial_Point,
				TARGET_DENSITY
			)	
			

		
			
	
	for( i in (2:(No_of_Steps + 1)  ))
	{

		if (Show_Details){ 
			print("===============================================================")
			print(c("Random Walk No ", i - 1 ))
		}
	
		New_Step_and_New_Log_Density <- 
				RANDOM_WALK(
						Chain[i-1,], 
						TARGET_DENSITY, 
						Problem_Dimension, 
						Log_Density_of_Current_State,
						Proposal_Covariance,
						Show_Details
				) 
		
			
		Chain[i,] 			<- New_Step_and_New_Log_Density[[1]]
		Log_Density_of_Current_State	<- New_Step_and_New_Log_Density[[2]]

		if (Show_Details){		
			print("State after Random Walk")
			print(Chain[i,])
		}			
	}
	
	return(Chain)
}


RANDOM_WALK	<- 	function(
				Current_State, 					#vector
				TARGET_DENSITY,					#function 
				Problem_Dimension, 				#int
				Current_Log_Density,				#numeric
				Proposal_Covariance,			 	#Matrix
				Show_Details					#boolean
				)
{
	Proposal_Step	<- Current_State + t(Proposal_Covariance %*% rnorm(Problem_Dimension))
					
	if (Show_Details){ print("Hello CHAIN_STEP") }
		
			# Logs of uniform distribution
			
	Ulog <- log( runif(1) )
	
			# Calculate  log pi(proposal) 
	
	if (Show_Details){ print(Proposal_Step)	}
		
	Proposal_Log_Density <- 
			UPDATING_LOG_OF_UNNORMALISED_DENSITY_IN_CURRENT_STATE(
				Proposal_Step,
				TARGET_DENSITY
			)	
				
	if (Show_Details){ 
		print("Current Log Densities")
		print(Current_Log_Density)
		print("Proposal Log Densities")
		print(Proposal_Log_Density)	
	}


		# It's Ulog < ... because then we must accept the proposals. 
				
	if (Show_Details){ 
		print("Ulog")	
		print(Ulog)
		print("Proposal_Log_Density - Current_Log_Density")
		print(Proposal_Log_Density - Current_Log_Density)
	}
		
	Did_we_move <- Ulog < (Proposal_Log_Density - Current_Log_Density )

	if (Show_Details){ 
		print("Did_we_move")
		print(Did_we_move)
	}
		# Updating the states that were accepted.
	
	if (Did_we_move) {	
		Current_State 		<- Proposal_Step 
		Current_Log_Density	<- Proposal_Log_Density 
	}

	
		# Preparing results
		
	Results_Enlisted 			<- 	vector("list", 3)
		
	Results_Enlisted[[1]] 			<- 	Current_State
	Results_Enlisted[[2]]			<-	Current_Log_Density  
	
	
 	return(	Results_Enlisted )
}

