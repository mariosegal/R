modeling <- read.csv("C:/Documents and Settings/ewnym5s/My Documents/Hudson City/modeling.csv")
modeling[is.na(modeling["IXI_tot"]),"IXI_tot"]<-colMeans(modeling["IXI_tot"],na.rm=T)
#reorder columns as I need the value we want to check correlations agaimnst to be the first column, also drop unwanted ones
modeling_small <- modeling[c("contrib","DDA_Amt","MMS_amt","sav_amt","TDA_Amt","IRA_amt","MTG_amt","HEQ_Amt","iln_amt","IXI_tot")]


"dda"       "mms"       "sav"       "tda"       "ira"       "mtg"       "heq"       "ILN"      
"deposits"  "loans"     "both"      "dep_amt"   "loan_amt"  "both_amt" 



library(lattice)

library(latticeExtra)
library(MASS)
library(lme4)
library(directlabels)


coeff <- as.vector(cor(x=modeling_small[2:10],y=modeling_small[1]))
coeff<- round(coeff,3)
names(coeff)<-names(modeling_small[2:10])

mypanel <- function(x,y,...) {
  panel.xyplot(x, y, ...)
  panel.grid(x=-1, y=-1)
  panel.lmline(x,y,col="red",lwd=1,lty=1)
  panel.text(12,60,bquote(rho == .(correls[panel.number()])),cex=.8, font = 2,col="black")
} 
data <- modeling_small[,2:10]
charts <- lapply(names(data), function(x) { xyplot (modeling_small[,1] ~ data[,x], 
                                                    panel=mypanel,ylab="MPG", xlab=x)})



 #assorted code below

substitute(R^2 == rrr, list(rrr=coeff[x]))
xyplot (mtcars$mpg ~ mtcars$cyl+ mtcars$wt,panel)

xyplot (mtcars$mpg ~ mtcars$cyl+ mtcars$wt,, panel=mypanel)

mpg <- mtcars[,1]

fit1 <- cor(mpg,data)
xyplot (mpg ~ data[,2])

for (i in 2:dim(mtcars)[2]){
  paste("chart",i,sep="") <- xyplot (mtcars[,1] ~ mtcars[,i], panel=mypanel)
}

panel.text(12,60,substitute(R^2 == rrr, list(rrr=r2[panel.number()])),cex=.8, font = 2,col="black")


load(mtcars)
mypanel <- function(x,y,...) {
  panel.xyplot(x,data[,y],...)
  panel.grid(x=-1,y=-1)
  panel.lmline(x,y,col="red",lwd=1,lty=1)
} 
data <- mtcars[,2:11]
charts <- apply(data,2,function(x) xyplot (mtcars[,1] ~ x, panel=mypanel,ylab="MPG",xlab=names(x)))


xyplot (mtcars[,1] ~ data, panel=mypanel,ylab="MPG"xlab=name(x))
