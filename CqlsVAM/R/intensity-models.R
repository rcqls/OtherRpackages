weibull.family.vam <- function(beta) {
	obj <- list(
			beta=beta,
			l=function(t) beta*t^(beta-1),
			L=function(t) t^beta,
			LInv=function(x) x^(1/beta),
			dl=function(t) beta*(beta-1)*t^(beta-2)
		)
	class(obj) <- c("weibull.family.vam","parametrized")
	obj
}

