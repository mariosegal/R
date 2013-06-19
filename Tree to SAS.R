tree2sas <- function (treeobj,predictor_name,filename) {
  

#initialize;
frame <- treeobj$frame
frame$printed <- 0;
frame$close <- 0;


code <- data.frame(line="",stringsAsFactors =F)
code <- data.frame(line="",stringsAsFactors =F)
j <- 1
count=0;
end <- row.names(frame[dim(frame)[1],]) 
mymax <- 0;
while (row.names(frame[i,]) != end) {
  i <- mymax+1
  #cycle the process until you get to the last leaf on the left from current node;
    while (frame[i,]$var != '<leaf>') {
      code[j,] = paste('if',frame[i,]$var,frame[i,]$splits[1,'cutleft'],'then do;')
      i<- i+1;
      j<-j+1;
    }
      
  mymax <- max(i,mymax)
      # we are at the farthest left possible, so write the assignment; 
      code[j,] = paste(predictor_name,' = ',"'",frame[i,]$yval,"'",";",sep="")
      #since I printed the prediction mark as printed;
      frame[i,7] <- 1;
      j<- j+1  
      
      #move up until you find the first not printed. To account for the way back from right;
      #check also if you are back at node #1 if not stop
       while (frame[i,]$printed==1) {
        if (i==1) {break}
         i<-i-1 
        if ( frame[i,]$close==0) {
          #if not not closed ,close it, I am closing leafs as I prinmt them so otherwise I duplicate;
          code[j,] = 'end;'
          j <- j+1;
          frame[i,8] <- 1;
        }
       }
  
      if (i == 1 & mymax==dim(frame)[1]) {break}
       code[j,] = paste('if',frame[i,]$var,frame[i,]$splits[1,'cutright'],'then do;')
       j<- j+1;
       frame[i,7] <- 1;
    #the next right one here will be max+1;

}

for (k in 1:dim(code)[1]) {
 cat(code[k,],sep="\n",append=T,file=filename)
}

}
