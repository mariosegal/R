
agecount <- function(age=NULL) {
  
  if(is.null(age)) {stop("An age is needed")}
  else {
    setwd("C:/Documents and Settings/ewnym5s/My Documents/R")
    homicides <- readLines("homicides.txt")
    
    

      r <- regexec("([1-9]*?) [yY]ears",homicides)
      q <- regmatches(homicides, r)
      p <- sapply(q,function(x){substr(x[2],1,999999)})
      p <- as.numeric(p)
      p <-p[!is.na(p)]
      t <- table(p)
      x <- t[as.character(age)]
      names(x) <- NULL
      if (is.na(x)) {x<- 0}
    
      return(x) }
    
  }
