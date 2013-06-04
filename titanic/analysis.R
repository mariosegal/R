setwd("/Users/mario/Dropbox/Kaggle/Titanic")
test<-read.csv("test.csv")
train <-read.csv("train.csv")

train$survived <- factor(train$survived,levels=c(0,1),labels=c("No","Yes"))
train$pclass <- factor(train$pclass,levels=c(1,2,3))

test$pclass <- factor(test$pclass,levels=c(1,2,3))
test$embarked <- factor(test$embarked,levels=c("","C","Q","S")) 

#do some initial exploratory analysis to get a fill for data;
dim(train)
table(train$survived)
sex <- prop.table(table(train$survived,train$sex),2)
class <- prop.table(table(train$survived,train$pclass),2)
sibsp <- prop.table(table(train$survived,train$sibsp),2)
parch <- prop.table(table(train$survived,train$parch),2)
embarked <- prop.table(table(train$survived,train$embarked),2)

sum(train$embarked == "")

#split train set into 2, so I can check model
set.seed(3467456)
n <- round(dim(train)[1]*0.75,0)
sample1 <- sample(1:dim(train)[1],n)
train1 <- train[sample1,]
validate <- train[-sample1,]


#try logistic regression #1
logmodel1 <- glm(survived ~ sex + pclass + age ,family="binomial",data=train1[complete.cases(train1[,c(1,2,4,5)]),]) 
summary(logmodel1)
predict1 <- logmodel1$fitted.values
predict1[predict1 > 0.5] <-1
predict1[predict1 <= 0.5] <-0
predict <- factor(predict1,levels=c(0,1),labels=c("No","Yes"))
plot(train1[complete.cases(train1[,c(1,2,4,5)]),]$survived,predict1,xlab="Actual",ylab="Predicted")
chartdata <- data.frame(actual=train1[complete.cases(train1[,c(1,2,4,5)]),]$survived,predicted=predict1,sex=train1[complete.cases(train1[,c(1,2,4,5)]),]$sex,pclass=train1[complete.cases(train1[,c(1,2,4,5)]),]$pclass)
prop.table(table(chartdata))
library(ggplot2)
ggplot(chartdata,aes(x=actual,y=predicted,color=sex))+geom_jitter()
ggplot(chartdata,aes(x=actual,y=predicted,color=pclass))+geom_jitter()

valid1<- predict(logmodel1,validate,type="response")
valid1[valid1 > 0.5] <-1
valid1[valid1 <= 0.5] <-0
valid1 <- factor(valid1,levels=c(0,1),labels=c("No","Yes"))


valid_data <- data.frame(actual=validate$survived,predicted=valid1)
ggplot(valid_data,aes(x=actual,y=predicted))+geom_jitter()
prop.table(table(valid_data))

testx <- test
testx$age[is.na(test$age)] <-  mean(train1$age,na.rm=T)
test_1<- predict(logmodel1,testx,type="response")
test_1[test_1 > 0.5] <-1
test_1[test_1 <= 0.5] <-0

write.table(test_1,file="submission_1",row.names=F,quote=F,col.names=F)

#do stepwise;
cols <- c(1,2,4,5,6,7,9,11)
logmodel2 <- glm(survived ~ (sex+pclass+age+sibsp+parch+fare+embarked)^2 ,family="binomial",data=train1[complete.cases(train1[,cols]),]) 
slm2 <- step(logmodel2)
predict2 <- slm2$fitted.values
predict2[predict2 > 0.5] <-1
predict2[predict2 <= 0.5] <-0
predict2 <- factor(predict2,levels=c(0,1),labels=c("No","Yes"))
chartdata2 <- data.frame(actual=train1[complete.cases(train1[,cols]),]$survived,predicted=predict2)
prop.table(table(chartdata2))

validx <- validate
validx$age[is.na(validx$age)] <-  mean(train1$age,na.rm=T)

valid2<- predict(slm2,validx,type="response")
valid2[valid2 > 0.5] <-1
valid2[valid2 <= 0.5] <-0
valid2 <- factor(valid2,levels=c(0,1),labels=c("No","Yes"))

valid_data2 <- data.frame(actual=validx$survived,predicted=valid2)
ggplot(valid_data2,aes(x=actual,y=predicted))+geom_jitter()
prop.table(table(valid_data2))


testx$fare[is.na(testx$fare)] <-  mean(train1$fare,na.rm=T)

test_2<- predict(slm2,testx,type="response")
test_2[test_2 > 0.5] <-1
test_2[test_2 <= 0.5] <-0

write.table(test_2,file="submission_2",row.names=F,quote=F,col.names=F)

#try to combine the 2, 
comp <- pmax(test_1,test_2)
write.table(comp,file="submission_3",row.names=F,quote=F,col.names=F)
#this did not imporve on the stepwise, taking all the 1 when 1 maybe makes no sense, or maybe with just 2 models it is stupid

#########
#Try a Tree

library(tree)

tree1 <- tree(survived ~ .,data=train[,c(-3,-8,-10)])
summary(tree1)
par(mfrow=c(1,2)) 
plot(cv.tree(tree1,FUN=prune.tree,method="misclass")) 
plot(cv.tree(tree1))

library(ggplot2)
library(ggdendro)

tree_data <- dendro_data(tree1)
ptree <- ggplot(segment(tree_data))+ggtitle("Tree Model Dendrogram")+theme(plot.title = element_text(size=14,color="blue", face="bold"))
ptree <- ptree + geom_segment(aes(x=x, y=y, xend=xend, yend=yend),colour="blue", alpha=0.5)
ptree <- ptree + geom_text(data=label(tree_data),aes(x=x, y=y, label=label), vjust=-0.5, size=3) 
ptree <- ptree + geom_text(data=leaf_label(tree_data),aes(x=x, y=y, label=label), vjust=0.5, size=4,face="bold",color="red") +theme_dendro()
ptree

tree2 <- tree(survived ~ sex+pclass+age+sibsp+parch+log(fare)+embarked,data=train[,c(-3,-8,-10)])
summary(tree2)
tree2
#log of fare did not really help, ended up same way





library(randomForest)
set.seed(876645)
forest1 <- randomForest(survived ~ .,data=train[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=3)
forest1

set.seed(876645)
forest2 <- randomForest(survived ~ .,data=train[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=3,ntree=2000)
forest2

valid_3 <- predict(forest2,newdata=validx)
valid_data3 <- data.frame(actual=validx$survived,predicted=valid_3)
prop.table(table(valid_data3))

test_3 <- predict(forest2,newdata=testx)
test_3a <- vector(mode="numeric",length=dim(testx)[1])
test_3a[test_3=="Yes"] <- 1

write.table(test_3a,file="submission_4",row.names=F,quote=F,col.names=F)


#what if I take out age;
set.seed(876645)
forest3 <- randomForest(survived ~ sex + pclass+sibsp+parch+fare ,data=train[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=4,ntree=2000)
forest3

valid_4 <- predict(forest3,newdata=validx)
valid_data4 <- data.frame(actual=validx$survived,predicted=valid_4)
prop.table(table(valid_data4))

test_4 <- predict(forest3,newdata=testx)
test_4a <- vector(mode="numeric",length=dim(testx)[1])
test_4a[test_4=="Yes"] <- 1

write.table(test_4a,file="submission_5",row.names=F,quote=F,col.names=F)

# this did improve, not sure why age out works but it does

#svm

svm1 <- svm(survived ~ (sex+pclass+age+sibsp+parch+fare+embarked)^2,data=train1)
pred <- predict(svm1,validx)
prop.table(table(pred,validx$survived))

test_5 <- predict(svm1,newdata=testx)
test_5a <- vector(mode="numeric",length=dim(testx)[1])
test_5a[test_5=="Yes"] <- 1
write.table(test_5a,file="submission_6",row.names=F,quote=F,col.names=F)

#this did work, not as good as the tree with no age, but theb naybe I take out age here as well

svm2 <- svm(survived ~ (sex+pclass+sibsp+parch+fare+embarked)^2,data=train1)
pred <- predict(svm2,validx)
prop.table(table(pred,validx$survived))
#this is not better on my validation sample than the first svm

#do my tree combination
train2 <- train[complete.cases(train[,c(-3,-5,-8,-10)]),c(-3,-5,-8,-10)]
combine <- matrix (data=0,nrow=dim(testx)[1],ncol=5)
for (i in 1:5) {
  temp_forest <- randomForest(survived ~ sex + pclass+sibsp+parch+fare ,data=train[,c(-3,-8,-10)],prox=T,na.action=na.omit,mtry=i,ntree=2000)
  pred <- predict(temp_forest,testx)
  pred_a <- vector(mode="numeric",length=dim(testx)[1])
  pred_a[pred=="Yes"] <- 1
  combine[,i] <- pred_a
}
library("modeest")

best <- vector(mode="numeric",dim(testx)[1])
for (i in 1:dim(testx)[1]) {
  best[i] <- mfv(combine[i,]) 
}

write.table(best,file="submission_7",row.names=F,quote=F,col.names=F)

#k fold
library("DAAG")
library("boot")
fit <- glm(survived ~ (sex+pclass+sibsp+parch+fare+embarked)^2  ,family="binomial",data=train2) 
set.seed(7878)
kfold1 <- CVbinary(fit,nfolds=8) 
   #this gave me an estimate fo the error on the real sample and it is useful to test my models,
   #but it is not combining 8 models for a prediction like I meant to do, so table below has 891 values whiohc is the trainign set


kpred <- vector(mode="numeric",dim(testx)[1])
temp <- predict(kfold1,testx)
kpred[kfold1$cvhat=="Yes"]<-1
write.table(kpred,file="submission_8",row.names=F,quote=F,col.names=F)
prop.table(table(kpred,train2$survived))

#medley? - did not work for classificatiom

x1<- train1[,c(-1,-3,-8,-10)]
y1 <- train1[,1]
y2 <- vector(mode="numeric",dim(train1)[1])
y2[y1=="Yes"] <- 1
m <- create.medley(x=x1, y=y2, errfunc=rmse);
for (mt in 1:5) {
  m <- add.medley(m, glm, list(formula=survived ~ (sex+pclass+age+sibsp+parch+fare+embarked)^2,data=train1,family="binomial"))
}

