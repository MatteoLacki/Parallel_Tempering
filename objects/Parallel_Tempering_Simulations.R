setClass(
	Class		= "Parallel_Tempering_Simulations",
	representation	= representation(
				No_of_Chains	= "numeric",					
        
					# Generated rather than provided.
				
        Initial_Points 	= "numeric",
				From_Lexic_Matrix = "matrix",
          
				Parallel_Tempering_Names_of_Slots_for_Table_in_Show_Method = "character",
			),

	prototype	= prototype(
				No_of_Chains	= numeric(0),
				
        # Generated rather than provided.
				
        Initial_Points 	= numeric(0),
				From_Lexic_Matrix = matrix(nrow=0, ncol=0),
        
        Parallel_Tempering_Names_of_Slots_for_Table_in_Show_Method  = c("Number_of_chains")
			),
	contains	= "Simulations"
)
	
	# Overrided methods

setMethod(
	f 		= "Make_a_Step_of_the_Algorithm",
	signature 	= "Parallel_Tempering_Simulations",

	definition	= function( simulation, ... ) { 
				print( "I am the step of Parallel Tempering algorithm." )	
			}	
)

	# Class specific methods

setGeneric(	"simulate",
		def = function( simulation, ... ){ 	standardGeneric("simulate") }
)

setMethod(
  "show",
  "Parallel_Tempering_Simulations",
  function( object ){
    cat("*** Showing details of Parallel_Tempering  ***\n")
    
      # Need to add a function that takes the names enlisted in the names_slots and takes all the objects, slots
      # and enlist their values where needed. But that is not needed now. First Make it display in the standard way. 
    PT_names <- object@Parallel_Tempering_Names_of_Slots_for_Table_in_Show_Method
    Details <- as.matrix( TO_DO() , nrow=length(PT_names), ncol=1 )
    
    colnames( Details ) <- 
    print(Details)
    
  }
)