df.list <- vector("list", 3) # create list
df.list
for(i in 1:3){df.list[[i]] <- matrix(data = i,
                                     nrow = i,
                                     ncol = 3,
                                     byrow = FALSE,
                                     dimnames = NULL)}

df.list
do.call(rbind, df.list) # rbind list elements

rep( list(Z), 3)

Liang_Means



Liang_Covariance_Matrices 	<- rep( list(diag(Liang_Sigma, nrow = 2, ncol = 2)) , times = Liang_No_of_Clusters )

sapply( Liang_Means_and_Covariances_Enlisted, function(x) { x[[1]] + chol(x[[2]]) %*% rnorm(n = length(x[[1]])) } )


x <- 2:3
sapply( Liang_Means_and_Covariances_Enlisted, function(y) dmvnorm(x, mean=y[[1]], sigma=y[[2]]) )

library(mvtnorm)

dmvnorm(1:2, 3:4, diag(3, ncol=2, nrow=2))


WEIGHTED_NORMAL_DISTRIBUTION( 2:3, Liang_Weights, Liang_Means_and_Covariances_Enlisted  )

sum(Liang_Weights * sapply( Liang_Means_and_Covariances_Enlisted, function(y) dmvnorm(x, mean=y[[1]], sigma=y[[2]]) ))


x <- seq(-2, 12, by=.2)
Z <- VALUE_ESTABLISHER(x, LIANG_TARGET_DENSITY)
persp(x,x, Z , theta = 12, phi = 30)


system.time(VALUE_ESTABLISHER(x, LIANG_TARGET_DENSITY))

# SOMETHING's Terribly wrong
COORDINATE_PROPOSITION(1:2, 2, LIANG_TARGET_DENSITY, diag(Liang_Temperatures[1]/16, nrow=2, ncol=2))



Liang_Initial_Points_with_Inverse_Temperatures_Enlisted[[1]][[2]]
A <- vector("list",5)


for (i in 1:5) {A[[i]] <- list(i, 1:2) }

x <- vector("list", 5)
for (i in 1:5) { x[[i]] <-PROPOSITION(A,LIANG_TARGET_DENSITY, diag(2, nrow=2, ncol=2))  }

x
x[[5]]

sim <- SIMULATION(6, Liang_Initial_Points_with_Inverse_Temperatures_Enlisted, PROPOSITION, LIANG_TARGET_DENSITY,  )


Liang_Initial_Info

SIMULATION( 10, Liang_Initial_Info, PROPOSITION , LIANG_TARGET_DENSITY)







rmvnorm(1, mean = 1:2, sigma = diag(2, nrow=2, ncol=2))

test <- matrix(1:20, nrow=2, ncol=10)

hello <- function(x) 
{
	return(rmvnorm(1, mean=x, sigma = diag(2, nrow=2,ncol =2))) 
}
apply(test, 2, hello)

test_2 	<- vector("list", 10)

for(i in 1:10) test_2[[i]] <- diag(Liang_Sigma, nrow=2, ncol=2)

test_3 <- rep(TRUE,3)
test_3[4] <- FALSE

test_3

z <- 1:4
y <- 5:8

z[test_3 == TRUE] <- y[test_3 == TRUE]


Liang_Chains_Covariance_Choleskised_Enlisted <- vector('list', Liang_No_of_Temperatures)

for (i in 1:Liang_No_of_Temperatures) Liang_Chains_Covariance_Choleskised_Enlisted[[i]] <- diag(.25*sqrt(Liang_Temperatures[i]), ncol=Liang_Dimension, nrow=Liang_Dimension)

Liang_Chains_Covariance_Choleskised_Enlisted

system.time(sapply(Liang_Chains_Covariance_Choleskised_Enlisted, function(x) x%*%rnorm(Liang_Dimension))) # No time Toulouse!

Z <- SPECIAL_PROPOSITION(matrix(rep(0, times=10), nrow=2, ncol=5), LIANG_TARGET_DENSITY, Liang_Chains_Covariance_Choleskised_Enlisted, Liang_No_of_Temperatures, Liang_Dimension, Liang_Inverse_Temperatures)

Z

Liang_Inverse_Temperatures * K

U <- log(runif(Liang_No_of_Temperatures))
U
cbind(diag(1,ncol= Liang_No_of_Temperatures, nrow=Liang_No_of_Temperatures), diag(-1,ncol= Liang_No_of_Temperatures, nrow=Liang_No_of_Temperatures)) %*% 


Z <- SPECIAL_PROPOSITION(matrix(rep(0, times=10), nrow=2, ncol=5), LIANG_TARGET_DENSITY, Liang_Chains_Covariance_Choleskised_Enlisted, Liang_No_of_Temperatures, Liang_Dimension, Liang_Inverse_Temperatures)

Z
Chaing <- vector('list',2)

Chaing[[1]] <- Z
Chaing

Z <- SIMULATION(10, Liang_Initial_Points, CHAIN_STEP, LIANG_TARGET_DENSITY, Liang_Proposals_Covariance_Choleskised_Enlisted, Liang_No_of_Chains, Liang_Problem_Dimension, Liang_Inverse_Temperatures) 


Z[[4]][,5:1]

TO_LEXIC(2,3,5)
FROM_LEXIC(TO_LEXIC(22,56,100),100 )

New_Log_Densities <- log(apply( Liang_Initial_Points, 2, LIANG_TARGET_DENSITY ))
New_Log_Densities

c(New_Log_Densities, New_Log_Densities)

log(apply( cbind(Liang_Initial_Points, Liang_Initial_Points ), 2, LIANG_TARGET_DENSITY ))

Liang_Initial_Points

Z <- function()
{
	SWAP_STEP(
		5,
		10,
		Liang_Initial_Points,
		DICTIONARY(5),
		New_Log_Densities,
		Liang_Inverse_Temperatures,
		STRATEGY_TWO,
		21
	)
}

Z() # Seems ok.

Z <- function()
{
	SWAP_STEP(
		5,
		10,
		Liang_Initial_Points,
		DICTIONARY(5),
		New_Log_Densities,
		Liang_Inverse_Temperatures,
		STRATEGY_THREE,
		21
	)
}

Z() == 1:5 # Seems ok

G <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_ONE,
				EASY_METRIC,
	Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

M <- G()
M

G <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				10,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_TWO,
				EASY_METRIC,
	Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

M <- G()
M

G <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_THREE,
				EASY_METRIC,
	Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

M <- G()

M

G <- function()
{
	return(
		SIMULATION(
				Liang_No_of_Chains,
				100,	# No of Steps of the algorithm
				Liang_Problem_Dimension,
				Liang_Initial_Points,
				LIANG_TARGET_DENSITY,
				STRATEGY_FOUR,
				EASY_METRIC,
	Liang_Proposals_Covariance_Choleskised_Enlisted,		
				Liang_Inverse_Temperatures		
		)
	)	
}

M <- G()

M

system.time(G())
Liang_Distribution_Values


library(graphics)



good_levels <- pretty(range(Liang_Distribution_Values, finite=TRUE), 20)
colorRampPalette(c('dark red','white','dark blue'))

filled.contour(x = Grid,
               y = Grid,
               z = Liang_Distribution_Values,
                 color.palette = cm.colors,
               levels = good_levels,
               col=terrain.colors(length(good_levels)-1)
               )

filled.contour(x = Grid,
               y = Grid,
               z = Liang_Distribution_Values,
               color.palette = cm.colors,
               levels = good_levels,
               col=colorRampPalette(c("white", "blue"), space = "Lab")(length(good_levels)-1)
              )

nlevels = 20
filled.contour(x = Grid,
               y = Grid,
               z = Liang_Distribution_Values,
               color.palette = cm.colors,
               levels = pretty(nlevels), nlevels = 20, 
               col=color.palette(length(nlevels)-1)
)


nlevels = 15
contour( 
          x = Grid,
          y = Grid,
          z = Liang_Distribution_Values,
          xlim = range(Grid, finite = TRUE),   
          ylim = range(Grid, finite = TRUE),   
          zlim = range(Liang_Distribution_Values, finite = TRUE),
          levels = pretty( range(Liang_Distribution_Values, finite = TRUE), nlevels)
      ) 

range(Grid, finite = TRUE)[1]



length(M)
help(points)


points(c(4,8)+c(range(Grid, finite = TRUE)[1]
,range(Grid, finite = TRUE)[1]
))

points(c(0,0))

matrix(nrow=2, ncol=2)

ls()


range(Liang_Distribution_Values, finite = TRUE),
help(pretty)
help(contour)

filled.contour(volcano,
               color.palette =
                 colorRampPalette(c("red", "white", "blue")),
               asp = 1)

filled.contour(volcano,
               color.palette =
                 colorRampPalette(c("red", "white", "blue"),
                                  space = "Lab"),
               asp = 1)


jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
filled.contour(volcano, color = jet.colors, asp = 1)

x <- colorRamp(c("red", "white", "blue"))

library(ggplot2)
library(reshape2)
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
head(volcano3d)

dim(Liang_Distribution_Values)

Liang_Distribution_Values_2   <- OTHER_VALUE_ESTABLISHER(Grid, LIANG_TARGET_DENSITY)

data <-as.data.frame( OTHER_VALUE_ESTABLISHER(Grid[1:5], LIANG_TARGET_DENSITY) )

names(data) <- c("x", "y", "z")
head(data)
v <- ggplot(data, aes(x, y, z =z )) 
v + stat_contour()

head(Liang_Distribution_Values_2)


data <-as.data.frame( OTHER_VALUE_ESTABLISHER(Grid, LIANG_TARGET_DENSITY) )

names(data) <- c("x", "y", "z")
write.csv2(data, "./Data/Liang_Density_Values_For_Contour_gg2plot.csv", row.names=FALSE, col.names=FALSE)


data <- read.csv2("./Data/Liang_Density_Values_For_Contour_gg2plot.csv")

d <- rnorm(100, mean=5, sd=1)

d <- d[d >=0 && d <= 10 ]
length(d)
data2 <- as.data.frame( 
                        matrix( d, 
                                nrow=50,
                                ncol=2
                        )  
                      )

data2 <- as.data.frame(matrix(c(5,5,2.5,7.5), nrow=2, ncol=2))

names(data2) <- c("x", "y")


head(data)
head(data2)
d <- qplot(data=data2, aes(x, y))


p <- qplot(x,y, data = data2)

p <- ggplot(data = data2, aes(x=x, y=y))
p + stat_contour(data=data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") + geom_point()
p + stat_contour(data=data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") + geom_point() + theme_bw() 
p + stat_contour(data=data,aes(x, y, z =z ), size=.5, colour="grey50") + geom_point()

p + stat_contour(data=data,aes(x, y, z =z ), bins=5, size=1) + stat_contour(data=data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") 
p + stat_contour(data=data,aes(x, y, z =z ), bins=20, size=.5, colour="grey50") 
p + stat_contour(data=data,aes(x, y, z =z ), bins=20, size=.5, colour=aes(fill=..level..), geom="polygon")

v <- ggplot(data, aes(x, y, z =z )) 
v + stat_contour() + theme_bw() 

+ geom_point(data = data2, aes(x=x, y=y))
length(M)
M
min(W)
max(W)

W <- 
t(	
	sapply(	M, 
		function(x){ 
			k <- numeric(10)
			k[(2*0:4 + 1)] <- x[1, 1:5]
			k[2*1:5] <- x[2,1:5]
			return(k)		
			} 
		)
)

dim(W)

k <- numeric(10)
k[(2*0:4 + 1)] <- z[1, 1:5]
k[2*1:5] <- z[2,1:5]

W <- PREPARE_DATA_FOR_2D_GGPLOT_CONTOUR( M, Liang_No_of_Chains, Liang_Problem_Dimension )

Main_Chain 		<- as.data.frame(W[,1:2])
names(Main_Chain) 	<- c("x", "y")

Second_Chain		<- as.data.frame(W[,3:4])
names(Second_Chain) 	<- c("x", "y")

replicate(5, c("x", "y"))

p <- ggplot(data = Main_Chain, aes(x=x, y=y))
p + stat_contour(data=data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") + geom_point(colour="red") + geom_point(data=Second_Chain,  colour="blue")

p + stat_contour(data=data,aes(x, y, z =z ), bins=10, size=.5, colour="grey50") + geom_point() + theme_bw() 
















