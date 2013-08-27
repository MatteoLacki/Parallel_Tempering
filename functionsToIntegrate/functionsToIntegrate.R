functionToIntegrate <- setRefClass(
	Class 	= "FunctionsToIntegrate",

###########################################################################
								# Fields	
	fields  = list(
		approximation 		= "numeric",
		integratedFunction	= "function"		
	),


###########################################################################
								# Methods
	
	methods = list(
		

		initialize = function(
			integratedFunction = NULL
		){
			if ( !is.null( integratedFunction )){
					# It is impossible to name the integrated function intergratedFunction
				integratedFunction <<- integratedFunction
			}	
		},	
	

		evaluate = function( argument ){
			return( integratedFunction( argument ) )		
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
