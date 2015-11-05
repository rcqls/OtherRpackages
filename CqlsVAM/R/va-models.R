#is.maintenance.va.model <- function(obj) {
#	!is.null(obj$maintenance.policy)
#}

ARA1.va.model <- function(rho,vam) {
	#Vright history not needed: the current updates from the previous one!
	obj <- new.env();obj$rho<-rho;obj$vam<-vam
	class(obj) <- c("ARA1","va.model")
	obj
}

params.ARA1 <- function(obj,param) {
	if(missing(param)) obj$rho
	else obj$rho <- param[1]
}

update.ARA1 <- function(obj,with.gradient=FALSE) {
	# next step
	obj$vam$model$k <- obj$vam$model$k + 1
	# At T(k)
	obj$vam$model$Vright <- obj$vam$model$Vright + (1-obj$rho)*(dVlr <-(obj$vam$model$Vleft-obj$vam$model$Vright))
	if(with.gradient) {
		# only the rho parameters
		#obj$vam$model$dVright <- obj$vam$model$dVright + rep(0,1+length(obj$vam$vam.PM$models))
		i <- match(obj$id,seq(obj$vam$vam.PM$models),nomatch=0)+1
		obj$vam$model$dVright[i] <- obj$vam$model$dVright[i] - dVlr
	}
	# save old model
	obj$model$mod <- obj
}

virtual.age.ARA1 <- function(obj,time) {
	max(0.0000001,obj$vam$model$Vright+time-obj$vam$data$Time[obj$vam$model$k])
}

virtual.age.derivative.ARA1 <- function(obj,time) {
	obj$vam$model$dVright
}

inverse.virtual.age.ARA1 <- function(obj,time) {
	time+obj$vam$data$Time[obj$vam$model$k]-obj$vam$model$Vright
}

ARAInf.va.model <- function(rho,vam) {
	#Vright history not needed: the current updates from the previous one!
	obj <- new.env();obj$rho<-rho;obj$vam<-vam
	class(obj) <- c("ARAInf","va.model")
	obj
}

update.ARAInf <- function(obj,with.gradient=FALSE) {
	# next step
	obj$vam$model$k <- obj$vam$model$k + 1
	# At T(k)
	obj$vam$model$Vright <- (1-obj$rho) * obj$vam$model$Vleft
	if(with.gradient) {
		# only with respect to the rho parameters (not with respect to beta)
		obj$vam$model$dVright <- (1-obj$rho) * obj$vam$model$dVright
		i<- match(obj$id,seq(obj$vam$vam.PM$models),nomatch=0)+1
		obj$vam$model$dVright[i] <-  obj$vam$model$dVright[i] - obj$vam$model$Vleft
	}
	# save old model
	obj$model$mod <- obj
}

params.ARAInf <- params.ARA1

virtual.age.ARAInf <-  virtual.age.ARA1

inverse.virtual.age.ARAInf <-  inverse.virtual.age.ARA1

virtual.age.derivative.ARAInf <- virtual.age.derivative.ARA1
