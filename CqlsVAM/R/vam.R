# Virtual Age Models (VAM)

# Simulation: sim.vam or vam.sim or vam.gen??? 

# data is here just in case you want to complete data (TODO later)
sim.vam <- function(formula,data) {
	# Have to generate both C + P
	# formula has to provide the two parts
	# sim.vam(~ Weibull(ARA1(.4) & (ARA1(.7) + ARAInf(.6) | periodic(100) ) ,1,2.5))
	# TODO: parsing formula later
	obj <- 	list(
				scale = 1,
				vam.CM=ara1.va.model(.4), #possibly a list
				vam.PM=list(ara1.va.model(.7),ara1.va.model(.6)),
				#policy.CM=list(weibull()),
				policy.PM=maintenance.policy.periodic(100,prob=c(.5,.5)),
				family=weibull.family.vam(1,2.5),
				ctxt=new.env()
			)


	class(obj) <- "sim.vam"
	obj
}

# stop.policy: number of events, events before time,....
simulate.sim.vam <- function(obj, nbsim=10, stop.time = Inf,seed = NULL) {
	# return a data.frame of the form
	# data.frame(Time=,Type=)
	Time <- 0 
	Type <- 1
	k <- 1
	Vp <- 0

	# stop.policy
	if(stop.time != Inf) {
		nbsim <- Inf
		stop.policy <- list(type="event",time=stop.time)
	} else {
		stop.policy<-list(type="counter",size=nbsim)
	}

	while(k < nbsim && Time[k] < stop.time) {
		mod <- if(Type[k]<0) obj$vam.CM else obj$vam.PM[[Type[k]]]



		time.CM <- mod$VInv(inverse.cummulative.density(obj$family,cummulative.density(obj$family,mod$V(Time[k],k,Time,Vp,mod$rho))-log(runif(1))),k,Time,Vp,mod$rho)
		
		# print(mod)
		# print(mod$V)

		# tmp <- mod$V(Time[k],k,Time,Vp,mod$rho)

		# print(tmp)

		# tmp<- cummulative.density(
		# 			obj$family,
		# 			tmp
		# 		)

		# tmp <- inverse.cummulative.density(
		# 		obj$family,
		# 		tmp-log(runif(1))
		# 		)

		# time.CM <-mod$VInv(tmp,k,Time,Vp,mod$rho)

		tmp.PM <- update(obj$policy.PM,Time[k]) # Peut-être ajout Vp comme argument de update


		k <- k+1

		# print(time.CM)
		# print(tmp.PM$time)

		if(time.CM < tmp.PM$time) {
			Time[k] <- time.CM
			Type[k] <- -1 #or -2, -3 dependening on the numbers of CM
			mod2 <- obj$vam.CM
		} else {
			Time[k] <- tmp.PM$time
			Type[k] <- tmp.PM$type
			mod2 <- obj$vam.PM[[Type[k]]]
		}
		Vp <- mod2$Vp(mod$V(Time[k],k,Time,Vp,mod$rho),Vp,mod2$rho)
	}

	res <- data.frame(Time=Time[-1],Type=Type[-1])
	attr(res,"stop.policy") <- stop.policy
	res
}

# Estimation part! The usual way in R

vam <- function(formula,data) {

}

summary.vam <- function(obj,...) {

}

