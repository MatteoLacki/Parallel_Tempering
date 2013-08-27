functionToIntegrate <- setRefClass(
	Class 	= "FunctionsToIntegrate",

###########################################################################
								# Fields	
	fields  = list(
		approximation 	= "numeric",
		integrant 	= "function"		
	),


###########################################################################
								# Methods
	
	methods = list(
		

		initialize = function(
			integrant = function(){}
		){
			integrant <<- integrant
		},	
	

		evaluate = function( argument ){
			return( integrant( argument ) )		
		},
	

		approximate = function( argument, iteration ){
			if( iteration == 1L ){
				approximation <<- evaluate( argument )
			} else {
				approximation <<- ((iteration-1)*approximation + evaluate( argument ))/iteration
			}
		},


		############################################################
				# Visualisation 

		show = function()
		{
			cat("\nThe estimated quantities are\n")
			print( approximation )
		}		
	)
)
