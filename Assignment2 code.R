#download.file("https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda",destfile="samsungData.rda",method="curl")
load("/Users/mario/Dropbox/Programming/data Analysis/Assignment 2/samsungData.rda")

#table(samsungData$subject)
#str(samsungData)


na1 <-is.na(samsungData)
sum(na1)
#create training, test an extra set;

names(samsungData) <- make.names(names(samsungData))
samsungData$activity <- factor(samsungData$activity)
training <- samsungData[samsungData$subject %in% c(1,3,5,6),-562]
testing <- samsungData[samsungData$subject %in% c(27,28,29,30),-562]
extra <- samsungData[!(samsungData$subject %in% c(1,3,5,6,27,28,29,30)),-562]

cor1 <- cor(training[,-562])

#do svd per Jeff's nptes;
svd1 <- svd(scale(training[,-c(562:563)]))

#i do not see anthing that seprates the moving to static ones, 
#so I am doing an svd only on the static  ones
static <- training[training$activity %in% c("laying","sitting","standing")
                   ,]
svd_static <- svd(scale(static[,-c(562:563)]))

#par(mfrow=c(2,2))
#plot(svd1$v[,1],pch=19)
#plot(svd1$v[,2],pch=19)
#plot(svd1$v[,3],pch=19)
#plot(svd1$v[,4],pch=19)

#create a df with the top variable in each right singular value vector;
maxmatrix <- data.frame(name=as.character(),value=as.numeric(),stringsAsFactors=F)
names(maxmatrix) <- c("name","value")
for (i in 1:561) {
  maxmatrix <- rbind(maxmatrix,data.frame(name=names(training)[which.max(svd1$v[,i])],value=max(svd1$v[,i])))
}

maxstatic <- data.frame(name=as.character(),value=as.numeric(),stringsAsFactors=F)
names(maxstatic) <- c("name","value")
for (i in 1:561) {
  maxstatic <- rbind(maxstatic,data.frame(name=names(static)[which.max(svd_static$v[,i])],value=max(svd_static$v[,i])))
}

#for the svd of all observations;
#identify fifth 10 and worst10 so i can create a chart and see the, 12 because they neatly fit in 3x4
ordered <- maxmatrix[order(-maxmatrix$value),]
best <- ordered[1:12,]
worst <- ordered[550:561,]
second <- ordered[13:24,]
third <- ordered[25:36,]
fourth <- ordered[37:48,]
fifth <- ordered[49:60,]

library(ggplot2)
for (i in 1:12) {
p <- ggplot(training,aes(x=activity,y=training[,maxmatrix[i,1]],color=training$activity))+geom_jitter()
p<- p<- p+scale_color_discrete(guide=FALSE)+ggtitle(maxmatrix[i,1])+theme(axis.title.y = element_blank(),plot.title = element_text(face="bold",size=8,color="red"))
assign(paste("best",i,sep=""),p)
}

#+
#ggtitle(paste("Predictive Rank = ",i,sep=""))
for (i in 550:561) {
  p <- ggplot(training,aes(x=activity,y=training[,maxmatrix[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(maxmatrix[i,1])+theme(axis.title.y = element_blank())
  assign(paste("worst",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(training,aes(x=activity,y=training[,second[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ylab(second[i,1])+ggtitle(second[i,1])+theme(axis.title.y = element_blank())
  assign(paste("second",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(training,aes(x=activity,y=training[,third[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ylab(third[i,1])+ggtitle(third[i,1])+theme(axis.title.y = element_blank())
  assign(paste("third",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(training,aes(x=activity,y=training[,fourth[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ylab(fourth[i,1])+ggtitle(fourth[i,1])+theme(axis.title.y = element_blank())
  assign(paste("fourth",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(training,aes(x=activity,y=training[,fifth[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ylab(fifth[i,1])+ggtitle(fifth[i,1])+theme(axis.title.y = element_blank())
  assign(paste("fifth",i,sep=""),p)
}

source("./dropbox/R/multiplot.R")
source("./dropbox/R/makeFootnote.R")
source("./dropbox/R/maketitle.R")


pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/best.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(best1,best2,best3,best4,best5,best6,best7,best8,best9,best10,best11,best12),cols=3)
makeTitle("Best 12 Variables",size=1.5)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/worst.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(worst1,worst2,worst3,worst4,worst5,worst6,worst7,worst8,worst9,worst10,worst11,worst12),cols=3)
makeTitle("Worst 12 Variables",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/second.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(second1,second2,second3,second4,second5,second6,second7,second8,second9,second10,second11,second12),cols=3)
makeTitle("Second 12  Best Variables",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/third.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(third1,third2,third3,third4,third5,third6,third7,third8,third9,third10,third11,third12),cols=3)
makeTitle("Third 12  Best Variables",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/fourth.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(fourth1,fourth2,fourth3,fourth4,fourth5,fourth6,fourth7,fourth8,fourth9,fourth10,fourth11,fourth12),cols=3)
makeTitle("Fourth 12  Best Variables",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/fifth.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(fifth1,fifth2,fifth3,fifth4,fifth5,fifth6,fifth7,fifth8,fifth9,fifth10,fifth11,fifth12),cols=3)
makeTitle("Fifth 12  Best Variables",size=2)
dev.off()

#for the svd of static observations;
#identify fifth 10 and worst10 so i can create a chart and see the top 12 
ordered1 <- maxstatic[order(-maxstatic$value),]
best1 <- ordered1[1:12,]
worst1 <- ordered1[550:561,]
second1 <- ordered1[13:24,]
third1 <- ordered1[25:36,]
fourth1 <- ordered1[37:48,]
fifth1 <- ordered1[49:60,]

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,maxstatic[i,1]],color=static$activity))+geom_jitter()
  p<- p<- p+scale_color_discrete(guide=FALSE)+ggtitle(maxstatic[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_best",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,worst1[i,1]],color=static$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(worst1[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_worst",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,second1[i,1]],color=static$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(second1[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_second",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,third1[i,1]],color=static$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(third1[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_third",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,fourth1[i,1]],color=static$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(fourth1[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_fourth",i,sep=""),p)
}

for (i in 1:12) {
  p <- ggplot(static,aes(x=activity,y=static[,fifth1[i,1]],color=static$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(fifth1[i,1])+theme(axis.title.y = element_blank())
  assign(paste("s_fifth",i,sep=""),p)
}

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/sbest.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_best1,s_best2,s_best3,s_best4,s_best5,s_best6,s_best7,s_best8,s_best9,s_best10,s_best11,s_best12),cols=3)
makeTitle("Best 12 Variables for Static",size=1.5)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/sworst.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_worst1,s_worst2,s_worst3,s_worst4,s_worst5,s_worst6,s_worst7,s_worst8,s_worst9,s_worst10,s_worst11,s_worst12),cols=3)
makeTitle("Worst 12 Variables for Static",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/ssecond.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_second1,s_second2,s_second3,s_second4,s_second5,s_second6,s_second7,s_second8,s_second9,s_second10,s_second11,s_second12),cols=3)
makeTitle("Second 12 Best Variables for Static",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/sthird.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_third1,s_third2,s_third3,s_third4,s_third5,s_third6,s_third7,s_third8,s_third9,s_third10,s_third11,s_third12),cols=3)
makeTitle("Third 12  Best Variables for Static",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/sfourth.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_fourth1,s_fourth2,s_fourth3,s_fourth4,s_fourth5,s_fourth6,s_fourth7,s_fourth8,s_fourth9,s_fourth10,s_fourth11,s_fourth12),cols=3)
makeTitle("Fourth 12 Best Variables for Static",size=2)
dev.off()

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/sfifth.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(s_fifth1,s_fifth2,s_fifth3,s_fifth4,s_fifth5,s_fifth6,s_fifth7,s_fifth8,s_fifth9,s_fifth10,s_fifth11,s_fifth12),cols=3)
makeTitle("Fifth 12 Best Variables for Static",size=2)
dev.off()


#I do not see any differentiators for static, I will see the common ones in static and all
t<-c(unique(maxmatrix[1:12,1]),unique(maxstatic[1:12,1]))
topvars <- unique(t)

#do tree wih the top variables from best all and best static
library(tree)


training_small <- training[,c(topvars,562)]
training_small$activity <- as.factor(training_small$activity)

set.seed(12345)
tree1 <- tree(activity ~ . ,data=training_small)
summary(tree1)



par(mfrow=c(1,2)) 
plot(cv.tree(tree1,FUN=prune.tree,method="misclass")) 
plot(cv.tree(tree1))


set.seed(12345)
tree2 <- tree(activity ~ . ,data=training)
summary(tree2)

plot(cv.tree(tree2,FUN=prune.tree,method="misclass")) 
plot(cv.tree(tree2))

par(mfrow=c(1,2)) 
plot(cv.tree(tree2,FUN=prune.tree,method="misclass")) 
plot(cv.tree(tree2))

chartdata <- data.frame(predicted=predict(tree2,type="class"),actual=training$activity)
fitchart <- ggplot(chartdata,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)
fitchart <- fitchart + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("'Large' Model Fit \non Training Sample")
fitchart <- fitchart+theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
fitchart <- fitchart+theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
fitchart <- fitchart+theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
fitchart <- fitchart + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
fitchart <- fitchart + scale_color_discrete(name="Actual\nActivity")
fitchart

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Figure 2.pdf",width=10,height=7,paper="USr",onefile=T)
fitchart
makeTitle("Figure 2: 'Large' Model Validation on Training Set",size=1.2)
makeFootnote_left("Figures Page 2 of 3")
makeFootnote_right("Mario Segal")
dev.off()


###############################################################

library(randomForest)

set.seed(17)
training.rf <- randomForest(activity ~ . ,data=training_small,importance=T,mtry=6)
print(training.rf)

set.seed(17)
training2.rf <- randomForest(activity ~ . ,data=training_small2,importance=T)
print(training2.rf)


set.seed(17)
training_all.rf <- randomForest(activity ~ . ,data=training,importance=T,mtry=6,proximity=TRUE)
print(training_all.rf)

par(mfrow=c(1,1)) 
plot(training_all.rf)
import <- importance(training_all.rf)

#test on validation sample
#test_clean <- test[,-562]
#test_clean$activity <- as.factor(test_clean$activity)
#names(test_clean) <-  make.names(names(test_clean))
#validate <- predict(training_all.rf, test, type="response")


#chart for validation sample
chartdata1 <- data.frame(predicted=training_all.rf$predicted,actual=training$activity)
fitchart1 <- ggplot(chartdata1,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)
fitchart1 <- fitchart1 + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("Random Forest Tree Fit \non Training Sample")
fitchart1 <- fitchart1+theme(legend.position="bottom")+theme(plot.title = element_text(size=18,color="blue", face="bold"))
fitchart1 <- fitchart1+theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
fitchart1 <- fitchart1+theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
fitchart1 <- fitchart1 + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
fitchart1 <- fitchart1 + scale_color_discrete(name="Actual\nActivity")
fitchart1

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Figure 3.pdf",width=10,height=7,paper="USr",onefile=T)
fitchart1
makeFootnote_left("Figures Page 3 of 3")
makeFootnote_right("Mario Segal")
dev.off()

#chart for test sample regular tree

testdata <- data.frame(predicted=predict(tree2,newdata=testing,type="class"),actual=testing$activity)
test <- ggplot(testdata,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)
test <- test + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("'Large' Model Fit \non Test Sample")
test <- test +theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
test <- test +theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
test <- test +theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
test <- test + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
test <- test + scale_color_discrete(name="Actual\nActivity")
test

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Figure 4.pdf",width=10,height=7,paper="USr",onefile=T)
test
makeFootnote_left("Figures Page 4 of 5")
makeFootnote_right("Mario Segal")
dev.off()


#chart for test sample forest 
validate <- predict(training_all.rf, newdata=testing, type="response")
testdata1 <- data.frame(predicted=validate,actual=testing$activity)
test1 <- ggplot(testdata1,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)
test1 <- test1 + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("Random Foest Tree Fit \non Test Sample")
test1 <- test1 +theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
test1 <- test1 +theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
test1 <- test1 +theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
test1 <- test1 + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
test1 <- test1 + scale_color_discrete(name="Actual\nActivity")
test1

bads <- testdata[testdata$predicted != testdata$actual,]
bads1 <- testdata[testdata1$predicted != testdata1$actual,]
errors <- c(dim(bads)[1],dim(bads1)[1])
errors <- errors/dim(testing)[1]
qplot(errors)

#chart of best 9 right sibgular vectors - all vars;

for (i in 1:12) {
  p <- ggplot(training,aes(x=activity,y=training[,best[i,1]],color=training$activity))+geom_jitter()
  p<- p+scale_color_discrete(guide=FALSE)+ggtitle(paste("Predictive Rank = ",i,sep=""))
  p<- p +theme(axis.text.x = element_text(angle=90),plot.title = element_text(face="bold",size=10,color="red"))
  if (i==2) {p<- p+ ylab("Variable value") + theme(axis.title.y = element_text(face="bold",size=14))}
  else {p<- p+theme(axis.title.y=element_blank())}
  if (i==6) {p<- p+ xlab("Activity") + theme(axis.title.x = element_text(face="bold",size=14))}
  else {p<- p+theme(axis.title.x=element_blank())}
  assign(paste("best",i,sep=""),p)
}

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Chart1.pdf",width=10,height=7,paper="USr",onefile=T)
multiplot(plotlist=list(best1,best2,best3,best4,best5,best6,best7,best8,best9),cols=3)
makeTitle("Figure 1: Top Predictive Variables from Right Singular Vectors",size=1.2)
makeFootnote_left("Figures Page 1 of 3")
makeFootnote_right("Mario Segal")
dev.off()
#try chart with facets, I need to extract the data and create a new df
best$newname <- make.names(best$name)
facetdata <- data.frame(activity=as.character(),variable=as.character(),value=as.numeric())
for (i in 1:9) {
  facetdata <- rbind(facetdata,data.frame(activity=training$activity,value=training[,maxmatrix[i,1]],variable=maxmatrix[i,1]))
}

p <- ggplot(facetdata,aes(x=activity,y=value,color=activity))+geom_jitter()+facet_wrap(~variable,ncol=3,scales="free_y",as.table=F)
p<- p+ggtitle("Figure 1: First 9 Top Predictive Variables from Right Singular Vectors")
p <- p + ylab("Measurement Value")+xlab("Type of Activity")+scale_color_discrete(name="Actual\nActivity")
p<- p+theme(axis.title.x=element_text(face="bold",size=12),axis.title.y=element_text(face="bold",size=12))
p<- p +theme(axis.text.x = element_text(angle=90,color="black"),plot.title = element_text(face="bold",size=14,color="blue"))
p <- p + theme(strip.text.x=element_text(color="black",face="bold"))+theme(legend.position="bottom")
p

pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Figure1.pdf",width=10,height=7,paper="USr",onefile=T)
p
makeFootnote_left("Figures Page 1 of 3")
makeFootnote_right("Mario Segal")
dev.off()



#create chart for validation of tree model using ggplot and not multiplot;
a <- data.frame(actual=training$activity,predicted=predict(tree2,newdata=training,type="class"),type="Training Sample")
b <- data.frame(actual=testing$activity,predicted=predict(tree2,newdata=testing,type="class"),type="Test Sample")
valid1 <- rbind(a,b)

validchart <- ggplot(valid1,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)+facet_grid(.~type)
validchart <- validchart + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("Figure 3: 'Large' Tree Model Fit on Training and Test Samples")
validchart <- validchart +theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
validchart <- validchart +theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
validchart <- validchart +theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
validchart <- validchart + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
validchart <- validchart + scale_color_discrete(name="Actual\nActivity")+theme(strip.text.x=element_text(color="red",face="bold",size=10))
validchart
          
#create chart for validation of forest model using ggplot and not multiplot;
#predicted=training_all.rf$predicted)
validate <- predict(training_all.rf, newdata=testing, type="response")
a <- data.frame(actual=training$activity,predicted=training_all.rf$predicted,type="Training Sample")
b <- data.frame(actual=testing$activity,predicted=validate,type="Test Sample")
valid2 <- rbind(a,b)

validchart1 <- ggplot(valid2,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)+facet_grid(.~type)
validchart1 <- validchart1 + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("Figure 4: Random Forest Fit on Training and Test Samples")
validchart1 <- validchart1 +theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
validchart1 <- validchart1 +theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
validchart1 <- validchart1 +theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
validchart1 <- validchart1 + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
validchart1 <- validchart1 + scale_color_discrete(name="Actual\nActivity")+theme(strip.text.x=element_text(color="red",face="bold",size=10))
validchart1


#do tree figure;
tree_data <- dendro_data(tree2)


new_labels <- vector(mode="character",length=11)
new_labels[1] <- "tBodyAcc.max...X < -0.552285"
new_labels[2] <- "tGravityAcc.mean...X \n< 0.30742"
new_labels[3] <- "tGravityAcc.max...Y  \n< -0.13075"
new_labels[4] <- "tGravityAcc.mean...Y  \n< -0.124436\n"
new_labels[5] <- "tGravityAcc.mean...Z  \n< 0.0286027"

new_labels[6] <- "tGravityAcc.min...Y \n< -0.297541"
new_labels[7] <- "fBodyAccJerk.bandsEnergy...17.32 \n< -0.536357"
new_labels[8] <- "fBodyAccMag.std..  \n< -0.329573"
new_labels[9] <- "fBodyAcc.bandsEnergy...17.24.1  \n< -0.808123\n"
new_labels[10] <- "tBodyAccJerk.max...X  \n< -0.443322\n"
new_labels[11] <- "tBodyAcc.max...X  \n< 0.0718084"

tree_data$labels$label <- new_labels

ptree <- ggplot(segment(tree_data))+ggtitle("Figure 2: 'Large' Tree Model Dendrogram")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
ptree <- ptree + geom_segment(aes(x=x, y=y, xend=xend, yend=yend),colour="blue", alpha=0.5)
ptree <- ptree + geom_text(data=label(tree_data),aes(x=x, y=y, label=label), vjust=-0.5, size=3) 
ptree <- ptree + geom_text(data=leaf_label(tree_data),aes(x=x, y=y, label=label), vjust=0.5, size=4,face="bold",color="red") +theme_dendro()
ptree




#make final figure pdf;
pdf("/Users/mario/dropbox/programming/data analysis/Assignment 2/Figures Final v2.pdf",width=10,height=7.5,paper="USr",onefile=T)
p
makeFootnote_left("Figures Page 1 of 5")
makeFootnote_right("Mario Segal")
ptree
makeFootnote_left("Figures Page 2 of 5")
makeFootnote_right("Mario Segal")
validchart
makeFootnote_left("Figures Page 3 of 5")
makeFootnote_right("Mario Segal")
validchart1
makeFootnote_left("Figures Page 4 of 5")
makeFootnote_right("Mario Segal")
MDSplot(training_all.rf,k=2, training$activity,palette=colors1,ylab="Dimension 1",xlab="Dimension 2")
title(main=list("Sample Multi Dimensional Scaling Plot \nof Proximity Matrix for Random Forest Model",col="blue",cex=1))
makeFootnote_left("Figures Page 4 of 5")
makeFootnote_right("Mario Segal")
dev.off()


#code to paste on document to reproduce random forest

#Load and Clean Data
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda",destfile="samsungData.rda",method="curl")
load("samsungData.rda")
names(samsungData) <- make.names(names(samsungData))
samsungData$activity <- factor(samsungData$activity)
training <- samsungData[samsungData$subject %in% c(1,3,5,6),-562]
testing <- samsungData[samsungData$subject %in% c(27,28,29,30),-562]

#Random Forest Generation
library(randomForest)
set.seed(17)
training_all.rf <- randomForest(activity ~ . ,data=training,importance=T,mtry=6,proximity=TRUE)

colors1 <- gg_color_hue(6)
MDSplot(training_all.rf,k=2, training$activity,palette=colors1)






title(main=list("Sample Multi Dimensional Scaling Plot \nof Proximity Matrix for Random Forest Model",col="blue",cex=1))

pc <- prcomp(training[,-562], scale=TRUE, center=TRUE, tol=0)
screeplot(pc, type="lines")
pc$rotation[,1]
pc1 <- pc$rotation[,1]
top1<-c(head(pc1,30),tail(pc1,30))
scatterplotMatrix(training[,names(top1)])

library(randomForest)
set.seed(67684)
rf_new <- randomForest(activity~.,data=training[,c(names(top1),"activity")])
rf_new
 #cool I identified top 60 variables and that seemed to do great;
#lets look at their plots
library(reshape)
library(ggplot2)

chartdata1 <- melt(training[,c(names(top1),"activity")],id.vars=c("activity"))
temp <- chartdata1[chartdata1$variable %in% names(top1)[49:60] & chartdata1$activity %in% c("walk","walkup","walkdown"),]

ch1 <- ggplot(data=temp,aes(x=activity,y=value,color=activity,facet=variable))+geom_jitter()+facet_wrap(~variable,ncol=3)
ch1

validate <- predict(rf_new, newdata=testing, type="response")
a <- data.frame(actual=training$activity,predicted=rf_new$predicted,type="Training Sample")
b <- data.frame(actual=testing$activity,predicted=validate,type="Test Sample")
valid2 <- rbind(a,b)

validchart_new <- ggplot(valid2,aes(x=actual,y=predicted,color=actual))+geom_jitter(alpha=.5)+facet_grid(.~type)
validchart_new <- validchart_new + ylab("Predicted Activity")+xlab("Actual Activity")+ggtitle("Figure 4: Random Forest Fit on Training and Test Samples")
validchart_new <- validchart_new +theme(legend.position="bottom")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
validchart_new <- validchart_new +theme(axis.title.x = element_text(face="bold",size=14),axis.title.y = element_text(face="bold",size=14))
validchart_new <- validchart_new +theme(axis.text.x=element_text(angle=90,color="black",size=12),axis.text.y=element_text(color="black",size=12))
validchart_new <- validchart_new + theme(legend.text = element_text(colour="black", size = 10),legend.title = element_text( face="bold"))
validchart_new <- validchart_new + scale_color_discrete(name="Actual\nActivity")+theme(strip.text.x=element_text(color="red",face="bold",size=10))
validchart_new

head(valid2)
dim(valid2[valid2$actual != valid2$predicted,])[1]/dim(valid2)[1]

