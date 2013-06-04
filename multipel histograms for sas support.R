test <- read.csv("C:\\Documents and Settings\\ewnym5s\\My Documents\\sheetz.csv")
test$atm_group <- as.factor(test$atm_group)
small <- test[test$atm_group == c(1,2) & test$sheetz_num <= 15,]

pdf("C:/Documents and Settings/ewnym5s/My Documents/sample.pdf",paper="USr",width=10,height=6.5)
  #chart1
ggplot(small, aes(x=sheetz_num,fill=atm_group)) + geom_histogram(alpha=.2, position="identity",binwidth=1)+labs(title="Chart1")

#chart2
ggplot(small, aes(x=sheetz_num,fill=atm_group)) + geom_histogram(alpha=1, position="dodge",binwidth=1)+labs(title="Chart2")

#chart3
ggplot(small, aes(x=sheetz_num,color=atm_group)) + geom_density()+labs(title="Chart3")

#chart4
ggplot(small, aes(x=sheetz_num)) + geom_histogram(alpha=.2, position="identity",binwidth=1)+labs(title="Chart4")+facet_grid(atm_group~.)


dev.off()
