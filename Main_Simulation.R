rm(list = ls())

directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#############################################################
getwd()
setwd(directory)
rm(directory)

#############################################################
# External libraries
library(mvtnorm)
#install.packages("mvtnorm")

	# My "libraries", a.k.a. pauper's makefile

source("./Distributions_to_check/tested_distribution.R")
source("./Functions/simulation_mechanism.R")
source("./Functions/additional_functions.R")
source("./Strategies_to_check/tested_strategies.R")
source("./Functions/original_Metropolis_Hasting.R")

ls()

#############################################################
# Possible Examples
	
	### Liang and Wong examplary density.

source("./Distributions_to_check/Liang_Example.R")
ls()


#############################################################
# Checking OOP.





rm(list = ls())

directory <- "/home/matteo/Documents/Scienza/Laurea_di_Matematica/Implementation"
#############################################################
getwd()
setwd(directory)
rm(directory)

source("./objects/Loading_Objects.R")


	# It works!
z <- new(
	"Parallel_Tempering_Simulations",
 	No_of_Steps=3, 
	Initial_Points=Liang_Initial_Points,
	Problem_Dimension=2,
	Target_Density=LIANG_TARGET_DENSITY
	)

Make_a_Step_of_the_Algorithm(z)
new("Parallel_Tempering_Simulations", No_of_Steps=3, Initial_Points= 5)

zw <- new("Metropolis_Hastings_Simulations", No_of_Steps=3, Initial_Point= 5)
Make_a_Step_of_the_Algorithm(zw)

zs <- new("Parallel_Tempering_Simulations", No_of_Steps=3)
Make_a_Step_of_the_Algorithm(zs)

