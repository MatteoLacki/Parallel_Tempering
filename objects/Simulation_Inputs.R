setClass(
	Class		= "Simulation_Inputs",
	representation	= representation(
				No_of_Steps = "numeric",
			),

	prototype	= prototype(N
				No_of_Steps 	= numeric(0),
				
			)
	# Add validator.
)