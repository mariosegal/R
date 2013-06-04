
svm3 <- svm(survived ~ (sex+pclass+sibsp+parch+fare+embarked)^2+(sex+pclass+sibsp+parch+fare)^3 ,data=train1,type="C-classification")

prop.table(table(svm3$fitted,train1$survived))

pred <- predict(svm3,testx)
pred_a <- vector(mode="numeric",length=dim(testx)[1])
pred_a[pred=="Yes"] <- 1

write.table(pred_a,file="submission_9.csv",row.names=F,quote=F,col.names=F)

combine1 <- matrix (data=0,nrow=dim(testx)[1],ncol=10)
for (i in 1:10) {
 # for (j in 1:10) {
    svm_x <- svm(survived ~ (sex+pclass+sibsp+parch+fare+embarked)^2+(sex+pclass+sibsp+parch+fare)^3 ,data=train1,type="C-classification",gamma=.001*i,cost=1)
    pred <- predict(svm_x,testx)
    pred_a <- vector(mode="numeric",length=dim(testx)[1])
    pred_a[pred=="Yes"] <- 1
    combine1[,(i-1)+1] <- pred_a
 # }
}

sum1 <- rowSums(combine1)
combo1 <- vector(mode="numeric",length=dim(testx)[1])
combo1[sum1>=5] <- 1;

write.table(combo1,file="submission_10.csv",row.names=F,quote=F,col.names=F)

combine2 <- matrix (data=0,nrow=dim(testx)[1],ncol=10)
for (i in 1:10) {
  # for (j in 1:10) {
  svm_x <- svm(survived ~ (sex+pclass+sibsp+parch+fare+embarked)^2+(sex+pclass+sibsp+parch+fare)^3 ,data=train1,type="C-classification",cost=i)
  pred <- predict(svm_x,testx)
  pred_a <- vector(mode="numeric",length=dim(testx)[1])
  pred_a[pred=="Yes"] <- 1
  combine2[,(i-1)+1] <- pred_a
  # }
}

sum2 <- rowSums(combine2)
combo2 <- vector(mode="numeric",length=dim(testx)[1])
combo2[sum2>=5] <- 1;

write.table(combo2,file="submission_11.csv",row.names=F,quote=F,col.names=F)


svm12 <- svm(survived ~ sex+pclass+sibsp+parch+fare+embarked ,data=train1,type="C-classification")

prop.table(table(svm12$fitted,train1$survived))

pred <- predict(svm3,testx)
pred_a <- vector(mode="numeric",length=dim(testx)[1])
pred_a[pred=="Yes"] <- 1

write.table(pred_a,file="submission_12.csv",row.names=F,quote=F,col.names=F)

#new ideas are - do more random forest (what to vary) - combine random forest with best svm I have with the bbest logistic I have
