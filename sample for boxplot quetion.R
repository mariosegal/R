data(mtcars)
data <- as.data.frame(mtcars)
data$cyl <- factor(data$cyl)
data$gear <- factor(data$gear)

summ <- ddply(data, .(cyl, gear),summarize, lower=quantile(mpg,probs=0.25,na.rm=T), middle=quantile(mpg,probs=.5,na.rm=T),upper=quantile(mpg,probs=.75,na.rm=T),avg=mean(mpg,na.rm=T))

p2 <- ggplot(summ, aes(x = cyl, lower = lower, middle = middle, upper = upper,fill=gear,ymin=lower,ymax=upper))+geom_boxplot(stat = "identity")
p2 <- p2 + geom_point(aes(x = cyl, y=avg, color=gear),color="red",position="dodge")
p2 