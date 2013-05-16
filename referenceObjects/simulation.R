#source("./objects/.R")

Simulation <- setRefClass(
	Class		= "Simulations",

	contains	= "VIRTUAL",

	fields		= list(
				# User provided
		noOfSteps   		= "integer",
		problemDimension	= "integer",
		targetDensity 		= "function",
		initialPoints		= "matrix",

			# Automatically initialized.
		Simulated_Chains	= "list",	# This should be another type of object.
		Current_States 		= "numeric",
		
			# Needed in iteration of the algorithm
		Log_Densities_of_Current_States	= "matrix"
	),

	methods 	= list(	
		
		initializeDataStructures = function()
		{
			cat("This will be written for more specific classes. It will generate basic data structures.\n")
		}

		

		initialize 	= function(
			noOfSteps 			= 0,
			problemDimension	= 0,
			targetDensity 		= 0,
			initialPoints		= matrix(nrow=0, ncol=0)
			)
		{
			correctnessCheck		<- TRUE	
			
			if ((noOfSteps < 0) || (noOfSteps %% 1 != 0)) 
			{
				stop("Inappropriate number of steps. Please enter an integer value")
			} else
			{	
				noOfSteps 		<<- noOfSteps
			}
			
			problemDimension	<<- problemDimension
			targetDensity 		<<- targetDensity
			
			initializeDataStructures( initialPoints )
		},		

		prototype	= prototype(
					noOfSteps 		= integer(0),
					Simulated_Chains 	= list(),
					targetDensity 		= function(){print("Hello. You should add a function.")},
					problemDimension 	= integer(0),
					initialPoints		= matrix(nrow=0, ncol=0),
					Current_States 		= numeric(0),
	
				 	Log_Densities_of_Current_States	= matrix(nrow=0, ncol=0),
				),
	
		validity 	= function( object )
			{
				# Optimal code: would check and write what is still not there.	
	
				cat("Inspector validates the data. \n")
				#if ( FALSE )
				#{
	    		#	stop("[Simulations: validation] you ommited some of necessary input.")
				#} else{}
				
				return(TRUE)	
			} 
	
	)
)

Simulation$lock
#source("./objects/Simulations[]Methods.R")