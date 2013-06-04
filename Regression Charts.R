data(mtcars)
panel.text(xmax[x],ymax,bquote(rho == .(round(cor(mtcars[,1], mtcars[,x]),3))))

mypanel <- function(x,y,...) {
  panel.xyplot(x, ...)
  panel.grid(x=-1, y=-1)
  panel.lmline(x,y,col="red",lwd=1,lty=1)
} 

mychart <- function(x,y) { 
  panel.grid(x=-1, y=-1)
  panel.xyplot (mtcars[,x] ,mtcars[,y]   ,ylab="MPG", xlab=x)
  panel.text(max(mtcars[,x]),max(mtcars[,y]),bquote(rho == .(round(cor(mtcars[,y], mtcars[,x]),3))))                        
}

mychart1 <- function(x) { 
  plot.new()
  xyplot (mtcars[,y]~mtcars[,x]    ,ylab="MPG", xlab=x)
  text(max(mtcars[,x]),max(mtcars[,1]),bquote(rho == .(round(cor(mtcars[,1], mtcars[,x]),3))))                        
}

panel.xyplot (mtcars[,x] ,mtcars[,y]   ,ylab="MPG", xlab=x,xlim=c(0,ceiling(max(mtcars[,x]))) ,ylim=c(0,ceiling(max(mtcars[,y]))))

data <- mtcars[,2:10]
charts <- lapply(names(data), mychart(x))

xnval <- 3
ynval <- 3
ncharts<- 9
for (i in 1:ncharts) {
  y = ceiling(i/xnval)
  if (i%%xnval!=0) {x=(i%%xnval)} else {x=xnval}
  if (i< ncharts) {
    print(charts[[i]],split=c(x,y,xnval,ynval),more=T) 
  }
  else {
    print(charts[[i]],split=c(x,y,xnval,ynval),more=F)
  }
}

correls <- as.vector(cor(x=mtcars[,2:10],y=mtcars[,1]))
correls<- round(correls,3)
names(correls)<-names(mtcars[,2:10])
xmax <-sapply(mtcars[,2:10],max)
names(xmax) <- names(mtcars[,2:10])
xmax<-floor(xmax)
ymax <- floor(max(mtcars[,1]))


data <- mtcars[,2:10]
charts <- lapply(names(data), function(x) { xyplot (mtcars[,1] ~ mtcars[,x], 
                                                    panel=mypanel,ylab="MPG", xlab=x,
                                                    xlim=c(0,ceiling(max(mtcars[,x])))
                                                  ,ylim=c(0,ceiling(max(mtcars[,1]))))})

old <- apply(names(data),2,function(x) {xyplot (mtcars[,1] ~ mtcars[,x], panel=mypanel,ylab="MPG",xlab=x,
                                                xlim=c(0,ceiling(max(mtcars[,x]))),
                                                ylim=c(0,ceiling(max(mtcars[,1])))}) 




bquote(rho == .(correls[x]))

