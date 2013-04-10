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

ls()

#############################################################
# Possible Examples
	
	### Liang and Wong examplary density.

source("./Distributions_to_check/Liang_Example.R")
ls()



	# Wanna see it? Why not !

Grid 				<- seq(-2, 12, by=.2)

	# It takes quite long to establish these 490 values of the target density. It's a bit bizarre.
#Liang_Distribution_Values 	<- VALUE_ESTABLISHER(Grid, LIANG_TARGET_DENSITY)

#write.csv2(Liang_Distribution_Values, "./Data/Liang_Density_Values.csv", row.names=FALSE, col.names=FALSE)
Liang_Distribution_Values   <- as.matrix(read.csv2("./Data/Liang_Density_Values.csv")	)
	
persp(Grid,Grid, Liang_Distribution_Values , theta = 12, phi = 30)

class(Liang_Distribution_Values)

	# Idea: we could even draw the points that were drawn from different phases of the algorithm.

#############################################################
#Simulation


# Ideas
# It would be instructive to show on a 2D map of our distribution to show how the algorithm behaves!
