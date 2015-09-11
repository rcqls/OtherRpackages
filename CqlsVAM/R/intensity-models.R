
Weibull.family.cm <- function(alpha,beta) {
	obj <- new.env()
	obj$alpha<-alpha;obj$beta <- beta #,
			#l=function(t) alpha*beta*t^(beta-1),
			#L=function(t) alpha*t^beta,
			#LInv=function(x) (x/alpha)^(1/beta),
			#dl=function(t) alpha*beta*(beta-1)*t^(beta-2)
		#)
	class(obj) <- c("Weibull.family.cm","parametrized")
	obj
}

params.Weibull.family.cm <- function(obj,param) {
	if(missing(param)) c(obj$alpha,obj$beta)
	else {
		obj$alpha <- param[1]
		obj$beta <- param[2]
	}
}

density.Weibull.family.cm <- function(obj,t) obj$alpha*obj$beta*t^(obj$beta-1)

cummulative.density.Weibull.family.cm <- function(obj,t) obj$alpha*t^obj$beta

inverse.cummulative.density.Weibull.family.cm <- function(obj,x) (x/obj$alpha)^(1/obj$beta)

density.derivative.Weibull.family.cm <- function(obj,t) obj$alpha*obj$beta*(obj$beta-1)*t^(obj$beta-2)

density.param.derivative.Weibull.family.cm <- function(obj,t) if(t==0) 0 else obj$alpha*(1+obj$beta*log(t))*t^(obj$beta-1)

cummulative.density.param.derivative.Weibull.family.cm <- function(obj,t) if(t==0) 0 else obj$alpha*log(t)*t^obj$beta