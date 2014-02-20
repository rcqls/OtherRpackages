getaddr <- function(x) {

  if (! is.vector(x)) stop("x should be a vector")

  addret <- integer(2)
  dyn.load(paste("getaddr",.Platform$dynlib.ext,sep=""))
  if (is.integer(x))  {  
    valret <- 0L
    out <- .C("getAddrForInt",x=x,addret=addret,valret=valret,DUP=FALSE)
  }
  if (is.double(x))   { 
    valret <- 0.0
    out <- .C("getAddrForDbl",x=x,addret=addret,valret=valret,DUP=FALSE)
  }
  dyn.unload(paste("getaddr",.Platform$dynlib.ext,sep=""))                               
  ## To improve for 64bits
  res <- sprintf("0x%X", as.integer(out$addret))


 obj<-list(addr.int=out$addret,addr.hex=res,value.at.addr=out$valret)
 class(obj) <- "ptrAddr"

 obj
  
}

Arith.ptrAddr <- function(e1,e2) {
  if(e2)

}

writeaddr <- function(addr,newval,at=0L) {

  #if (length(addr) != 1) stop("addr should be of length 1")
  if (length(newval) != 1) stop("newval should be of length 1")
  addr.int <- as.integer(addr$addr.int)
  addr.int[1] <- addr.int[1]+at
  dyn.load(paste("getaddr",.Platform$dynlib.ext,sep=""))
  if (is.integer(newval)) {
    .C("writeAtAddrForInt",addr.int,newval,DUP=FALSE)
  }

  if (is.double(newval)) {
    .C("writeAtAddrForDbl",addr.int,newval,DUP=FALSE)
  }
  dyn.unload(paste("getaddr",.Platform$dynlib.ext,sep=""))
}


x <- 1L
x
getAddr(x)


addr <- getAddr(x)
addr # adresse (exprimée en valeur entière) de la première case mémoire du bloc mémoire de 8 octets contenant x
newval <- 3L
writeAddr(addr,newval)
x


x <- 2.75
addr <- getAddr(x)
addr # L'adresse a changé car on a changé le type de x
#newval <- pi
writeAddr(addr,pi)
x

getAddr(x)

x <- c(3L,4L)
x
addr <- getAddr(x) # Récupère l'adresse de la première case du bloc de cases où est stocké x
writeAddr(addr,6L);x
writeAddr(addr,7L,4L);x
