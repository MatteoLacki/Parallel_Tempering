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