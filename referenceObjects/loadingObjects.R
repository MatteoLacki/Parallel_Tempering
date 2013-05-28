directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
setwd(directory)
source("./referenceObjects/simulations.R")
source("./referenceObjects/parallelTempering.R")


########################################################### TESTS

Aha <- function(x){ return(dnorm(x)) }

z <- parallelTemperingSimulation$new(
	noOfSteps	=10000,
	targetDensity	=Aha, 
	problemDimension=2, 
	temperatures	= c(1.4, 5.4, 6.9, 10) 
)



length(NA)
length(c(NA,NA)) # to jest problem..


length(c())
