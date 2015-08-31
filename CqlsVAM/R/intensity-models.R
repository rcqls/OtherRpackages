cummulative.density <- function(x,...) UseMethod("cummulative.density") 

inverse.cummulative.density <- function(x,...) UseMethod("inverse.cummulative.density") 

density.derivative <- function(x,...) UseMethod("density.derivative") 


weibull.family.vam <- function(alpha,beta) {
	obj <- list(
			alpha=alpha,
			beta=beta #,
			#l=function(t) alpha*beta*t^(beta-1),
			#L=function(t) alpha*t^beta,
			#LInv=function(x) (x/alpha)^(1/beta),
			#dl=function(t) alpha*beta*(beta-1)*t^(beta-2)
		)
	class(obj) <- c("weibull.family.vam","parametrized")
	obj
}

density.weibull.family.vam <- function(obj,t) obj$alpha*obj$beta*t^(obj$beta-1)

cummulative.density.weibull.family.vam <- function(obj,t) obj$alpha*t^obj$beta

inverse.cummulative.density.weibull.family.vam <- function(obj,x) (x/obj$alpha)^(1/obj$beta)

density.derivative.weibull.family.vam <- function(t) obj$alpha*obj$beta*(obj$beta-1)*t^(obj$beta-2)

