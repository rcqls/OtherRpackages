#is.maintenance.va.model <- function(obj) {
#	!is.null(obj$maintenance.policy)
#}

virtual.age <- function(x,...) UseMethod("virtual.age") 

inverse.virtual.age <- function(x,...) UseMethod("inverse.virtual.age")

ARA1.va.model <- function(rho,vam) {
	#Vp history not needed: the current updates from the previous one!
	obj <- list(rho=rho,vam=vam)
	class(obj) <- c("ARA1","va.model")
	obj
}

update.ARA1 <- function(obj) {
	obj$vam$cache$Vp <- (1-obj$rho)*(virtual.age(obj$vam$cache$mod,obj$vam$Time[obj$vam$cache$k])-obj$vam$cache$Vp) + obj$vam$cache$Vp
}

virtual.age.ARA1 <- function(obj,time) {
	obj$vam$cache$Vp+time-obj$vam$Time[obj$vam$cache$k]
}

inverse.virtual.age.ARA1 <- function(obj,time) {
	time+obj$vam$Time[obj$vam$cache$k]-obj$vam$cache$Vp
}

ARAInf.va.model <- function(rho,vam) {
	#Vp history not needed: the current updates from the previous one!
	obj <- list(rho=rho,vam=vam)
	class(obj) <- c("ARAInf","va.model")
	obj
}

update.ARAInf <- function(obj) {
	obj$vam$cache$Vp <- (1-obj$rho)*virtual.age(obj$vam$cache$mod,obj$vam$Time[obj$vam$cache$k])
}

virtual.age.ARAInf <-  virtual.age.ARA1

inverse.virtual.age.ARAInf <-  inverse.virtual.age.ARA1


