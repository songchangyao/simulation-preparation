getwd()

####Exercise 1: Get started in R
data()
library(datasets)
data("morley")
attach(morley)
View(morley)
rm(list = ls(all=TRUE))

####Exercise 2: Getting your Data and Weights into R

getwd()
setwd("D:/RStudio/R study/simulation preparation/Data")
police <- read.csv("police.csv")
View(police)
summary(police)

library(spdep)

##2.4.1 using read.gal
polgal <- read.gal("policerook.GAL",override.id = TRUE)
attributes(polgal)
print(polgal)

##2.4.2 weights characteristics
summary(polgal)

##2.5.1 using read.gwt2nb
attach(police)
summary(POLICE)

polgwt <- read.gwt2nb("policek5.GWT",region.id=FIPSNO)
summary(polgwt)

##2.5.2 checking for symmetry of a neighbour list
print(is.symmetric.nb(polgal))

print(is.symmetric.nb(polgwt))

rm(list = ls(all=TRUE))

####Exercise 3: Spatial autocorrelation-Analysis in R
spdep()
data("columbus")
attach(columbus)

### 3.2 Converting neighbor lists to spatial weights

colqueen <- nb2listw(col.gal.nb)
class(colqueen)
summary(colqueen)
View(colqueen)
colqueen$weights

###3.3 Moran's I
mornorINC <- moran.test(INC,colqueen,randomisation=FALSE,alternative="two.sided")
print(mornorINC)
class(mornorINC)
mornorINC$statistic
moran.test(CRIME,colqueen,alternative="two.sided")     ##randomisation can be omitted
moran.test(CRIME,colqueen)

##3.3.2 permutation inference
set.seed(123456)  ###make sure the outcome identical

morpermCRIME <- moran.mc(CRIME,colqueen,99)
morpermCRIME
morpermCRIME$res

##3.3.3 Plotting the reference distribution
morp <- morpermCRIME$res[1:length(morpermCRIME$res)-1]
zz <- density(morp)

plot(zz,main="Moran's I Permutation Test",
             xlab="Reference Distribution",ylab="Density",
             xlim=c(-0.3,0.7),ylim=c(0,6),lwd=2,col=2)
hist(morp,freq=F,add=T)
abline(v=morpermCRIME$statistic,lwd=2,col=4)
postscript(file="filename")
pdf(file="filename")
postscript(file="moran.pdf")




###3.4 Constructing a spatially lagged variable (lag.listw)
xx <- cbind(CRIME,INC,HOVAL)      #截取变量
xx
wxx <- lag.listw(colqueen,xx)     #生成滞后项
wxx
attributes(colqueen)
colqueen$neighbours[1]
colqueen$weights[1]
(xx[2,] + xx[3,])/2.0

###3.5 Moran scatter plot (moran.plot)
moran.plot(CRIME,colqueen)

##3.5.2 Customizing the Moran Scatter Plot
x <- CRIME
zx <- (x - mean(x))/sd(x)
mean(zx)
var(zx)
wfile <- colqueen
wzx <- lag.listw(wfile,zx)
morlm <- lm(wzx ~ zx)
aa <- morlm$coefficients[1]
mori <- morlm$coefficients[2]
aa
mori
#mapping
par(pty="s") # make sure it's square
plot(zx,wzx,xlab="CRIME",ylab="Spatial Lag of CRIME")
abline(aa,mori)
abline(h=0,lty=2)
abline(v=0,lty=2)
title(paste("Moran Scatterplot I= ",format(round(mori,4))))

moran.plot2 <- function(x,wfile)     #生成函数
{
  xname <- deparse(substitute(x)) # get name of variable
  zx <- (x - mean(x))/sd(x)
  wzx <- lag.listw(wfile,zx)
  morlm <- lm(wzx ~ zx)
  aa <- morlm$coefficients[1]
  mori <- morlm$coefficients[2]
  par(pty="s")
  plot(zx,wzx,xlab=xname,ylab=paste("Spatial Lag of ",xname))
  abline(aa,mori,col=2)
  abline(h=0,lty=2,col=4)
  abline(v=0,lty=2,col=4)
  title(paste("Moran Scatterplot I= ",format(round(mori,4))))
}
moran.plot2(CRIME,colqueen)
moran.plot2(INC,colqueen)
