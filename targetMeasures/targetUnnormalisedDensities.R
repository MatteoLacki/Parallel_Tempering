targetUDensity <- setRefClass(
	Class		= "TargetUnnormalisedDensities",
	contains	= "TargetMeasures",

###########################################################################
								# Fields
	fields		= list(
		targetUDensity 	= "function"	
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function(
			targetDensity = function(){}
		)
		{
			targetUDensity <<- targetDensity 
		},

		############################################################
				# Visualisation

		show = function()
		{
			cat('\nThe general target unnormalised density inputs are here: \n')
			cat("The target unnormalised density function:\n", targetUDensity)
		},		


		############################################################
				# Algorithmic Methods				

		measure 	= function(
			proposedState
		)
		{	
			return(	targetUDensity( proposedState ) )
		}
####################################################################
				# Finis Structurae		
	)
)