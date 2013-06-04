splom(#You need to create an object called my data with your data.frame
#the process will create charts of correlations for the first column versus all others
#and then arrage them in a lattice patter.
#It uses the multiplot fucntion that I found as well as ggplot2

mydata<-mtcars
modeling_small <- mydata
rand1 <- sample(1:1000000,size=500000)
mydata <-  modeling_small[rand1,]

library(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

mychart <- function(x) {
  c <- round(cor(mydata[,1],mydata[x]),3)
  xmax <-ceiling(max(mydata[,x]))
  xmin <- floor(min(mydata[,x]))
  xpos = floor(max(mydata[,x])*(8/10))
  ypos = floor(max(mydata[,1])*(8/10))
  t = paste("rho ==",c,sep="")
  t1 <- annotate("text",x=xpos,y=ypos,label=t,parse=TRUE,color="red")
  p <- qplot(mydata[,x],mydata[,1],xlab=x,ylab=names(mydata)[1],color=I("blue"))
  s <- stat_smooth(aes(x=mydata[,x],y=mydata[,1]),method="lm",color="red",se=FALSE)
  a <- annotate("text",x=xpos,y=ypos,label=t,parse=TRUE,color="red")
  p<- p+s+a+xlim(xmin,xmax)

}

rand1 <- sample(1:1000000,size=100000)
mydata <-  modeling_small[rand1,]

charts <- NULL
l1 <- NULL
for (i in 2:(length(mydata))) {
  charts[[i]]<- mychart(names(mydata)[i])
}

numcols <- ceiling(sqrt(length(mydata)-1))

multiplot(plotlist=charts[2:length(mydata)],cols=numcols)
