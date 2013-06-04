#what if I select a random sample with replacement eact time and do not use a valuidation sample, and then
#average say multiple trees or svm or logistics 

library(randomForest)

tuneRF(train1[,c(-1,-5,-3,-8,-10)], train1[,1], mtryStart=5, ntreeTry=2000, stepFactor=1, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE)

#it seems 1 is the best mtry, the later it appears 5 is best;
results <- matrix (data=0,nrow=dim(testx)[1],ncol=100)
set.seed(456)

for (i in 1:100) {
  sample1 <- sample(1:dim(train)[1],dim(train)[1],replace=T)
  t1 <- train[sample1,] 
  f1 <- randomForest(survived ~ sex + pclass+sibsp+parch+fare ,data=t1[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=5,ntree=2000)
  p1 <- predict(f1,testx)
  p2 <- vector(mode="numeric",dim(testx)[1])
  p2[p1=="Yes"] <- 1
  results[,i] <- p2
}
total <- rowSums(results)
submit <- vector(mode="numeric",dim(testx)[1])
submit[total >= 50] <- 1
write.table(submit,file="submission_14.csv",row.names=F,quote=F,col.names=F)

#####
results <- matrix (data=0,nrow=dim(testx)[1],ncol=100)
set.seed(456)

for (i in 1:100) {
  sample1 <- sample(1:dim(train)[1],700,replace=F)
  t1 <- train[sample1,] 
  f1 <- randomForest(survived ~ sex + pclass+sibsp+parch+fare ,data=t1[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=5,ntree=2000)
  p1 <- predict(f1,testx)
  p2 <- vector(mode="numeric",dim(testx)[1])
  p2[p1=="Yes"] <- 1
  results[,i] <- p2
}
total <- rowSums(results)
submit <- vector(mode="numeric",dim(testx)[1])
submit[total >= 50] <- 1
write.table(submit,file="submission_14.csv",row.names=F,quote=F,col.names=F)


