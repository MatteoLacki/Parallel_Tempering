strategy <- setRefClass(
	Class		= "Strategies",
	contains	= "VIRTUAL",

###########################################################################
								# Fields
	fields		= list(

		transpositionsNo	 = "integer"
		
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

		returnSwap	= function(
			transpositions
		)
		{}

####################################################################
				# Finis Structurae		
	)
)