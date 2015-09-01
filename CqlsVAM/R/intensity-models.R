cummulative.density <- function(x,...) UseMethod("cummulative.density") 

inverse.cummulative.density <- function(x,...) UseMethod("inverse.cummulative.density") 

density.derivative <- function(x,...) UseMethod("density.derivative") 


Weibull.family.cm <- function(alpha,beta) {
	obj <- list(
			alpha=alpha,
			beta=beta #,
			#l=function(t) alpha*beta*t^(beta-1),
			#L=function(t) alpha*t^beta,
			#LInv=function(x) (x/alpha)^(1/beta),
			#dl=function(t) alpha*beta*(beta-1)*t^(beta-2)
		)
	class(obj) <- c("Weibull.family.cm","parametrized")
	obj
}

density.Weibull.family.cm <- function(obj,t) obj$alpha*obj$beta*t^(obj$beta-1)

cummulative.density.Weibull.family.cm <- function(obj,t) obj$alpha*t^obj$beta

inverse.cummulative.density.Weibull.family.cm <- function(obj,x) (x/obj$alpha)^(1/obj$beta)

density.derivative.Weibull.family.cm <- function(t) obj$alpha*obj$beta*(obj$beta-1)*t^(obj$beta-2)

