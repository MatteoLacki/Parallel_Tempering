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




