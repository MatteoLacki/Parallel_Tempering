targetMeasure <- setRefClass(
	Class		= "TargetMeasures",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

			## Real values of the density values pre-evaluated in a square [-2,12]^2.
		realDensityValues = "data.frame"
	),

###########################################################################
								# Methods

	methods 	= list(

		############################################################
				# Initialisation

		initialize 	= function()
		{},

		############################################################
				# Visualisation

		############################################################
				# Algorithmic Methods				

		measure 	= function()
		{
			return( NULL )
		},

		establishTrueValues = function()
		{			
		}
####################################################################
				# Finis Structurae		
	)
)