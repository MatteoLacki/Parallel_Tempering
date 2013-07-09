stateDependentStrategy <- setRefClass(
	Class		= "stateDependentStrategies",
	contains	= c("Strategies", "VIRTUAL"),

###########################################################################
								# Fields
	fields		= list()	
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
)`