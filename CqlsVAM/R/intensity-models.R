
Weibull.family.cm <- function(alpha,beta) {
	obj <- new.env()
	obj$alpha<-alpha;obj$beta <- beta
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

density.Weibull.family.cm <- function(obj,t) if(t<=0) 0 else obj$alpha*obj$beta*t^(obj$beta-1)

cumulative.density.Weibull.family.cm <- function(obj,t) obj$alpha*t^obj$beta

inverse.cumulative.density.Weibull.family.cm <- function(obj,x) (x/obj$alpha)^(1/obj$beta)

density.derivative.Weibull.family.cm <- function(obj,t) obj$alpha*obj$beta*(obj$beta-1)*t^(obj$beta-2)

density.param.derivative.Weibull.family.cm <- function(obj,t) if(t==0) 0 else obj$alpha*(1+obj$beta*log(t))*t^(obj$beta-1)

cumulative.density.param.derivative.Weibull.family.cm <- function(obj,t) if(t==0) 0 else obj$alpha*log(t)*t^obj$beta

## Thanks to Cecile Chauvel
LogLinear.family.cm <- function(alpha,beta) {
  obj <- new.env()
  obj$alpha<-alpha;obj$beta <- beta 
  class(obj) <- c("LogLinear.family.cm","parametrized")
  obj
}

params.LogLinear.family.cm <- function(obj,param) {
  if(missing(param)) c(obj$alpha,obj$beta)
  else {
    obj$alpha <- param[1]
    obj$beta <- param[2]
  }
}

density.LogLinear.family.cm <- function(obj,t) if(t<=0) 0 else exp(obj$alpha+obj$beta*t)

cumulative.density.LogLinear.family.cm <- function(obj,t) exp(obj$alpha)*(exp(obj$beta*t)-1)/obj$beta

inverse.cumulative.density.LogLinear.family.cm <- function(obj,x) log(1+x*obj$beta*exp(-obj$alpha))/obj$beta

density.derivative.LogLinear.family.cm <- function(obj,t) obj$beta*exp(obj$alpha+obj$beta*t)

density.param.derivative.LogLinear.family.cm <- function(obj,t) t*exp(obj$alpha+obj$beta*t)

cumulative.density.param.derivative.LogLinear.family.cm <- function(obj,t) exp(obj$alpha)*(t*exp(t*obj$beta)/obj$beta-(exp(obj$beta*t)-1)/obj$beta^2)

