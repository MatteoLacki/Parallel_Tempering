targetMeasure <- setRefClass(
	Class		= "TargetMeasures",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(),

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
		}
####################################################################
				# Finis Structurae		
	)
)