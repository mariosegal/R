library("RODBC")
myconn <- odbcConnect(dsn="SASODBC",believeNRows=FALSE, colQuote=NULL)
train <- sqlQuery(myconn,"select * from clv.train_steady_small")
odbcCloseAll()