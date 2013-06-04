#read the data from the csv export
modeling <- read.csv("C:/Documents and Settings/ewnym5s/My Documents/Hudson City/modeling.csv")

#transform the segment variabkles into 0/1 indicators and since we do not have 2 for hudson transform to 4 and other transformations, 


segm <- modeling$segment
segm[segm==2] <- 4
segm[segm==8] <- 1

check <- function (x,y) {
  vec1 <- vector(mode = "logical", length = length(x))
  for(i in 1:length(x)) {
    vec1[i] <- FALSE
    if (!is.na(x[i])) {
      if (x[i]==y) {vec1[i]<- TRUE}
    }
  
  }
  return(vec1)
}

check1 <- function (x,y) {
  
  if (!is.na(x)) {
    if(x == y) {return(TRUE)}
    else {return(FALSE)}
  }
  else {return(FALSE)}
}

s1 <- check(segm,1)
s3 <- sapply(segm,check1,y=3)
s4 <- sapply(segm,check1,y=4)
s5 <- sapply(segm,check1,y=5)
s6 <- sapply(segm,check1,y=6)
s7 <- sapply(segm,check1,y=7)

modeling$s1 <- s1
modeling$s3 <- s3
modeling$s4 <- s4
modeling$s5 <- s5
modeling$s6 <- s6
modeling$s7 <- s7

modeling$iln_amt[is.na(modeling$iln_amt)] <- 0

ixi_new <- modeling$IXI_tot
ixi_new[is.na(ixi_new)] <- mean(ixi_new,na.rm=T)
modeling$ixi_new <- ixi_new


rm(list=c('s5','s6','s7','s1','s3','s4','segm','ixi_new'))



chunk1 <- modeling[1:200000,]
chunk2 <- modeling[200001:400000,]
chunk3 <- modeling[400001:600000,]
chunk4 <- modeling[600001:800000,]
chunk5 <- modeling[800001:1000000,]
a <- biglm(contrib ~ dda + mms + sav + tda + ira + mtg + ILN + heq + deposits + DDA_Amt + MMS_amt + sav_amt + TDA_Amt +
             IRA_amt + MTG_amt + HEQ_Amt + iln_amt + cqi_DD + A + B + C + D + E + both + dep_amt + loan_amt + both_amt + 
             loans + deposits +s1 +s3 +s4+ s5+ s6+s7 +ixi_new, chunk1)

b <- update (a,chunk2)
c <- update (b,chunk3)
d <- update (c,chunk4)
final_fit <- update(d,chunk5)

fit <- lm(contrib ~ dda + mms + sav + tda + ira + mtg + ILN + heq + deposits + DDA_Amt + MMS_amt + sav_amt + TDA_Amt +
        IRA_amt + MTG_amt + HEQ_Amt + iln_amt + cqi_DD + A + B + C + D + E + both + dep_amt + loan_amt + both_amt + 
            loans + deposits +s1 +s3 +s4+ s5+ s6+s7 +ixi_new, modeling)
