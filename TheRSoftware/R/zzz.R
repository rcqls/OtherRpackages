.onLoad <- function(libname,pkg){
  library.dynam("TheRSoftware", pkg, libname)
}

.onAttach <- function(libname, pkg){
  packageStartupMessage(utils::packageDescription('TheRSoftware',lib.loc = libname),appendLF=TRUE)
}
