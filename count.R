#setwd("C:/Documents and Settings/ewnym5s/My Documents/R")
#homicides <- readLines("homicides.txt")


count <- function(cause=NULL) {
  
  if(is.null(cause)) {stop("A cause of death is needed")}
  else {
        cause <-tolower(cause)
        allowed= c("shooting","blunt force","other","unknown","stabbing","asphyxiation")
    if (cause %in% allowed) { 
      r <- regexec("[Cc]ause:(.*?)</",homicides)
      q <- regmatches(homicides, r)
      p <- sapply(q,function(x){substr(x[2],2,999999)})
      s <- sub("^[Ss]hooting","shooting",p,ignore.case=T)
      s <- sub("^[Bb]lunt [Ff]orce","blunt force",s,ignore.case=T)
      s <- sub("^[Aa]sphyxiation","asphyxiation",s,ignore.case=T)
      s <- sub("^[Oo]ther","other",s,ignore.case=T)
      s <- sub("^[Ss]tabbing","stabbing",s,ignore.case=T)
      s <- sub("^[Uu]nknown","unknown",s,ignore.case=T)
      t <- table(s)
      x <- t[cause]
      names(x) <- NULL
      return(x) }
    else {
      #cause was not allowed
      stop("Cause of death must be:shooting,blunt force,other,unknown,stabbing or asphyxiation")
    }
}
}