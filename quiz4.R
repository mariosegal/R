download.file("https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt","movies.txt",method="auto")
movies <- read.table("https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt",sep="\t",header=T,quote="")

names(movies)

qplot(x=score,y=box.office,data=movies)
cor(movies$score,movies$box.office)
fit <- lm(movies$score~movies$box.office)
summary(fit)

qplot(x=score,y=running.time,data=movies)
fit1 <- lm(movies$score~movies$box.office + movies$running.time)
summary(fit1)

movies.new <- movies[movies$running.time < 200,]
fit2 <- lm(movies.new$score~movies.new$box.office + movies.new$running.time)
summary(fit2)

fit3 <- lm(movies$score~ movies$running.time+movies$box.office)
summary(fit3)

library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

fit4 <- lm(movies$score~ movies$running.time*movies$rating)
summary(fit4)
p4<- predict(fit4)
qplot(x=movies$score,y=p4)+geom_abline(intercept=0,slope=1,color="Red")
b <- fit4$coefficients
qplot(movies$running.time,movies$box.office)



data(warpbreaks)
fit5 <- lm(warpbreaks$breaks~warpbreaks$tension)
summary(fit5)