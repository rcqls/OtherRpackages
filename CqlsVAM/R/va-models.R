

ara1.va.model <- function(rho,maintenance.policy=NULL,vam.context=NULL) {
	#Vp history not needed: the current updates from the previous one!
	obj <- list(
			rho=rho,
			maintenance.policy=maintenance.polycy,
	 		Vp=expression(Vp <- (1-rho)*(Time[k]-Time[k-1])+Vp), # Vp[k] <- (1-rho)*(Time[k]-Time[k-1])+Vp[k-1]
			V=expression(Vp+time-Time[k]),
			VInv=expression(time+Time[k]-Vp)
		)
	class(obj) <- c("ara1","va.model")
	obj
}

araInf.va.model <- function(rho,maintenance.policy=NULL) {
	#Vp history not needed: the current updates from the previous one!
	obj <- list(
			rho=rho,
			maintenance.policy=maintenance.policy,
	 		Vp=expression(Vp <- (1-rho)*(Time[k]-Time[k-1]+Vp)), # Vp[k] <- (1-rho)*(Time[k]-Time[k-1]+Vp[k-1])
			V=expression(Vp+time-Time[k]),
			VInv=expression(time+Time[k]-Vp)
		)
	class(obj) <- c("araInf","va.model")
	obj
}

### type=1 or Inf
# ara.va.model <- function(typeC=1,rhoC,typeP=1,rhoP) {
# 	env <- new.env()
# 	env$rhoC <- rhoC
# 	env$rhoP <- rhoP
# 	env$typeC <- typeC
# 	env$typeP <- typeP
# 	#VPlus history not needed! Only the last previous one required!
# 	obj <- list(
# 			env=env,
# 	 		VpInf=expression(Vp[k] <- ((Type[k] < 0)*(1-rhoC) +  (Type[k] > 0) * (1-rhoP[Type[k]]))*(Time[k]-Time[k-1]+Vp[k-1])),
# 			VpOne=expression(Vp[k] <- ((Type[k] < 0)* (Type[k] < 0)*(1-rhoC) +  (Type[k] > 0) * (1-rhoP[Type[k]]))*(Time[k]-Time[k-1])+Vp[k-1]),
# 			V=expression(Vp[k]+time-Time[k]),
# 			VInv=expression(time+Time[k]-Vp[k])
# 		)
# 	class(obj) <- c("ara","va.model")
# 	obj
# }

