S1_10000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				10000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_ONE,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}


S2_10000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				10000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_TWO,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

S3_10000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				10000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_THREE,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

S4_10000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				10000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_FOUR,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}


S1_100000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_ONE,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}


S2_100000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_TWO,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

S3_100000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_THREE,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

S4_100000 <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100000,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_FOUR,
				EASY_METRIC,
				Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}


overnight_simulation <- function()
{
	write.csv2(S1_10000, "./Data/S1_10000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S2_10000, "./Data/S2_10000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S3_10000, "./Data/S3_10000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S4_10000, "./Data/S4_10000.csv", row.names=FALSE, col.names=FALSE)

	write.csv2(S1_100000, "./Data/S1_100000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S2_100000, "./Data/S2_100000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S3_100000, "./Data/S3_100000.csv", row.names=FALSE, col.names=FALSE)
	write.csv2(S4_100000, "./Data/S4_100000.csv", row.names=FALSE, col.names=FALSE)
}

overnight_simulation()


