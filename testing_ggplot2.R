a <- seq(1,20)
b <- a^0.25
plot(a,b, bty = "l")

library(ggplot2)

df <- as.data.frame(cbind(a,b))

# 1. ggplot2 default
ggplot(df, aes(x = a, y = b)) + geom_point()

# 2. removes background color
ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_rect(fill='white', colour='black'))

# 3. also removes gridlines
none <- element_blank()
ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_blank(fill='white', colour='black')) + theme(panel.grid.major = none, panel.grid.minor = none)

# 4. does not remove top and right border
ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_blank(fill='white', colour='black')) + theme(panel.grid.major = none, panel.grid.minor = none) + theme(panel.border = none)

# 5. does not remove top and right border
ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_blank(fill='white', colour='black')) + theme(panel.grid.major = none, panel.grid.minor = none) + theme(axis.line = theme_segment())

# 6. removes x and y axis in addition to top and right border
# http://stackoverflow.com/questions/5458409/remove-top-and-right-border-from-ggplot2
ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_blank(fill='white', colour='black')) + theme(panel.grid.major = none, panel.grid.minor = none) + theme(panel.background=element_blank(colour=NA))

# 7. returns error when attempting to remove top and right border
# https://groups.google.com/group/ggplot2/browse_thread/thread/f998d113638bf251
#
# Error in el(...) : could not find function "polylineGrob"
#
theme_L_border <- function(colour = "black", size = 1, linetype = 1) { 
  structure( 
    function(x = 0, y = 0, width = 1, height = 1, ...) { 
      polylineGrob( 
        x=c(x+width, x, x), y=c(y,y,y+height), ..., default.units = "npc", 
        gp=gpar(lwd=size, col=colour, lty=linetype), 
      ) 
    }, 
    class = "theme", 
    type = "box", 
    call = match.call() 
  )
}

ggplot(df, aes(x = a, y = b)) + geom_point() + theme(panel.background = element_blank(fill='white', colour='black')) + theme(panel.grid.major = none, panel.grid.minor = none) + theme( panel.border = theme_L_border())

x = Grid,
y = Grid,
z = Liang_Distribution_Values,

help(melt)
library(reshape2)
melt(volcano)
ggplot2()

volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")


v <- ggplot(volcano3d, aes(x, y, z = z))
v + stat_contour()
summary(volcano)
head(volcano)
head(melt(volcano))


head(Liang_Distribution_Values)
melt(Liang_Distribution_Values)

Z <- Liang_Distribution_Values
Z

colnames(Z) <- c()

dim(Liang3D)

colnames(Liang3D) <- c("x","y","z")



v <- ggplot(Liang3D, aes(x, y, z = z))
v + stat_contour()

v<- ggplot(aes(x=Grid, y=Grid, z=Liang_Distribution_Values))
