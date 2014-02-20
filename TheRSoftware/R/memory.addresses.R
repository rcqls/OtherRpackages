getaddr <- function(x) {
  
  if (! is.vector(x)) stop("x should be a vector")
  
  addret <- 0L
  if (is.integer(x))  {
    valret <- 0L
    out <- .C("getaddrint",x=x,addret=addret,valret=valret,DUP=FALSE,PACKAGE="TheRSoftware")
  }
  if (is.double(x))   {
    valret <- 0.0
    out <- .C("getaddrdble",x=x,addret=addret,valret=valret,DUP=FALSE,PACKAGE="TheRSoftware")
  }
  res <- sprintf("0x%X", as.integer(addret))
  
  return(list(addr.int=out$addret,addr.hex=res,value.at.addr=out$valret))
  
}


writeaddr <- function(addr,newval) {

  if (length(addr) != 1) stop("addr should be of length 1")
  if (length(newval) != 1) stop("newval should be of length 1")
  addr <- as.integer(addr)
  
  if (is.integer(newval)) {
    .C("writeataddrint",addr,newval,DUP=FALSE,PACKAGE="TheRSoftware")
  }
  
  if (is.double(newval)) {
    .C("writeataddrdble",addr,newval,DUP=FALSE,PACKAGE="TheRSoftware")
  }
}
