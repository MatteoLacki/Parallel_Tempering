realFiniteDimensionalStateSpaceStructure <- setRefClass(
	Class		= "realFiniteDimensionalStateSpaceStructure",
	contains	= "stateSpaceStructure",

		############################################################	

	fields		= list(

		simultatedStates	= "matrix",
		currentStatesLogDensities= "matrix",
		currentStates 		= "matrix",
		proposalsCovarianceCholeskised = "matrix",
		quasiMetric  		= "function",
		translatorFromLexicOrderToTranspositions = "matrix"
	),	
	

		############################################################

	methods 	= list(

	)
)