# type=1 or Inf

ara.va.model <- function(typeC=1,rhoC,typeP=1,rhoP) {
	env <- new.env()
	env$rhoC <- rhoC
	env$rhoP <- rhoP
	env$typeC <- typeC
	env$typeP <- typeP
	#VPlus history not needed! Only the last previous one required!
	obj <- list(
			env=env,
	 		VpInf=expression(Vp[k] <- ((Type[k] < 0)*(1-rhoC) +  (Type[k] > 0) * (1-rhoP[Type[k]]))*(Time[k]-Time[k-1]+Vp[k-1])),
			VpOne=expression(Vp[k] <- ((Type[k] < 0)* (Type[k] < 0)*(1-rhoC) +  (Type[k] > 0) * (1-rhoP[Type[k]]))*(Time[k]-Time[k-1])+Vp[k-1]),
			V=expression(Vp[k]+time-Time[k]),
			VInv=expression(time+Time[k]-Vp[k])
		)
	class(obj) <- c("ara","va.model")
	obj
}