
insertInitialStates
	input:
		initialStates 		= matrix(ncol=0, nrow=0),
			spaceDim			= 0L,
			chainsNo			= 0L
	output:	
		spaceDim =
		chainsNo =
		lastStates = 


insertStates 
	input:
		freeSlotNo = 
		slotsNo =
		spaceDim = 
		lastStates = 
	output:
		simulatedStates = 

createDataStorage	
	input: 	
		spaceDim = 
		iterationsNo =
		chainsNo = 
	output:
		freeSlotNo = 
		slotsNo =




insertProposalCovariances
	input:
		proposalCovariances = 
	output:
		simpleProposalCovariance =
		proposalCovariancesCholeskised = 