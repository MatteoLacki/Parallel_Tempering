	# Adding this in R is for strictly logical reasons. In C++ we would require the 
	# existence of several methods.

stateSpaceStructure <- setRefClass(
	Class		= "stateSpaceStructure",
	contains	= "VIRTUAL",

	fields		= list(),
	methods 	= list()
)