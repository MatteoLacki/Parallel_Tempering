#source("./objects/.R")

Simulation <- setRefClass(
	Class		= "Simulations",

	contains	= "VIRTUAL",

	fields		= list(
				# User provided
		noOfSteps   		= "integer",
		problemDimension	= "integer",
		targetDensity 		= "function"
		#initialPoints		= "matrix",	# I don't need that. Should be passed simply as first object in the chain implementation.

			# Automatically initialized.
					# Needed in iteration of the algorithm : it is used for every possible thing.
		#Log_Densities_of_Current_States	= "matrix"
	),

	methods 	= list(	
		
		#initializeDataStructures = function( 
		#	initialPoints		= matrix(nrow=0, ncol=0) 
		#	)
		#{
		#	cat("This will be written for more specific classes. It will generate basic data structures.\n")
		#},

		initialize 				= function(
			noOfSteps 			= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			#initialPoints		= matrix(nrow=0, ncol=0),
			...
			)
		{
			simulationInitializator(
				noOfSteps 		=noOfSteps, 
				problemDimension=problemDimension,
				targetDensity 	= targetDensity,
				...
				#initialPoints 	= initialPoints
				)

			#initializeDataStructures( initialPoints )
		},

		simulationInitializator	= function(
			noOfSteps 			= 0L,
			problemDimension	= 0L,
			targetDensity 		= NULL,
			#initialPoints		= matrix(nrow=0, ncol=0),
			...
			)
		{
			tmpNoOfSteps 	<- as.integer(noOfSteps)
			
			if ( is.na(tmpNoOfSteps) || (noOfSteps < 0) ) 
			{
				stop("Inappropriate number of steps. Please enter an integer value.")
			} else
			{	
				noOfSteps 		<<- tmpNoOfSteps
			}

			tmpProblemDimension	<- as.integer(problemDimension)

			if ( is.na(tmpNoOfSteps) || (noOfSteps < 0) ) 
			{
				stop("Inappropriate problem dimension. Please enter an integer value.")
			} else
			{	
				problemDimension 		<<- tmpProblemDimension
			}

			targetDensity 		<<- targetDensity
		},	

		makeStepOfTheAlgorithm	= function()
		{
			cat('I shall make it all happen.')
		},

		simulationShow = function()
		{
			cat('\n Welcome to our simulation! \n')
			cat('Number of steps: ', noOfSteps, '\n')
			cat('Dimension of the problem: ', problemDimension, '\n')
		},	

		show	= function()
		{
			simulationShow()
		},

		simulate = function()
		{
			for ( iteration in 1:noOfSteps ) makeStepOfTheAlgorithm() 
				# Another reason to introduce a separate data structure.
		}	

	)
)

	# This will lock all fields apart those enlisted.
Simulation$lock( names( Simulation$fields() ) )