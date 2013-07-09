x <- as.data.frame( 
read.csv2(
	"./data/LiangTrueValues.csv",
	header = TRUE
	) 
)
head(x)

x <- x[,2:4]
head(x)

y <- x[x$z<.05,]
dim(y)
dim(x)

max(y$z)

plot(y$x, y$y)

w <- x[x$z==0,]
plot(w$x, w$y)
