# Virtual Age Models (VAM)

# Simulation: sim.vam or vam.sim or vam.gen??? 

# data is here just in case you want to complete data (TODO later)
sim.vam <- function(formula,data) {
	# Have to generate both C + P
	# formula has to provide the two parts
	# sim.vam(~ (ARA1(.4) | Weibull(1,2.5)) & (ARA1(.7) + ARAInf(.6) | Periodic(100,prob=c(.5,.5)) ) )
	# TODO: parsing formula later
	# obj <- 	list(
	# 			scale = 1,
	# 			vam.CM=list(list(model=ARA1.va.model(.4),family=weibull.family.cm(1,2.5))), #possibly a list
	# 			vam.PM=list(models=list(ARA1.va.model(.7),ARA1.va.model(.6)),policy=maintenance.policy.Periodic(100,prob=c(.5,.5)))
	# 		)

	obj <- parse.sim.vam.formula(formula)

	# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????

	class(obj) <- "sim.vam"
	obj
}

parse.sim.vam.formula <- function(formula) {
	if(formula[[1]] != as.name("~")) stop("Not a formula as argument")
	if(length(formula) != 2) stop("No left part in the formula!")
	cm <- formula[[2]]
	if(cm[[1]] == as.name("&")) { # there is a PM part
		pm <- cm[[3]]
		cm <- cm[[2]]
		# deal with PM part
		pms <- list()
		if(pm[[1]] == as.name("(")) {
			pm <- pm[[2]]
			if(pm[[1]] != as.name("|")) stop("Need a policy to manage Preventive Maintenance")
			policy <- pm[[3]]
			if(is.name(policy[[1]])) {
				policy[[1]] <- as.name(paste0(as.character(policy[[1]]),".maintenance.policy"))
			}
			policy <- eval(policy)
			# PMs
			pm <- pm[[2]]
			# parser for pm
			parse.pm <- function(pm) {
				if(is.name(pm[[1]])) {
					pm[[1]] <- as.name(paste0(as.character(pm[[1]]),".va.model"))
				}
				eval(pm)
			}
			cpt.pms <- 0
			while(pm[[1]] == as.name("+") ) {
				if(length(pm) == 3) {
					pms[[cpt.pms <- cpt.pms + 1]] <- parse.pm(pm[[3]])
					pm <- pm[[2]]
				}
			}
			pms[[cpt.pms <- cpt.pms + 1]] <- parse.pm(pm)
		} else stop("Need a policy to manage Preventive Maintenance")
	}
	# deal with CM PART
	cms <- list()
	 
	# parser for cm
	parse.cm <- function(cm) {
		if(cm[[1]] != as.name("(")) stop("CM needs a family!")
		cm <- cm[[2]]
		if(cm[[1]] != as.name("|")) stop("CM needs a family!")
		family <- cm[[3]]
		if(is.name(family[[1]])) {
			family[[1]] <- as.name(paste0(as.character(family[[1]]),".family.cm"))
		}
		cm <- cm[[2]]
		if(is.name(cm[[1]])) {
			cm[[1]] <- as.name(paste0(as.character(cm[[1]]),".va.model"))
		}
		list(model=eval(cm),family=eval(family))
	}
	cpt.cms <- 0
	while( cm[[1]] == as.name("+") ) {
		if(length(cm) == 3) {
			cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm[[3]])
			cm <- cm[[2]]
		}
	}
	cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm)

	
	# return
	list(vam.CM=cms[rev(seq(cms))],vam.PM=list(models=pms[rev(seq(pms))],policy=policy))
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

	# when concurrency between CMs occurs this line would be inside the loop
	family <- obj$vam.CM[[1]]$family

	while(k < nbsim && Time[k] < stop.time) {
		mod <- if(Type[k]<0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[Type[k]]]

		time.CM <- mod$VInv(inverse.cummulative.density(family,cummulative.density(family,mod$V(Time[k],k,Time,Vp,mod$rho))-log(runif(1))),k,Time,Vp,mod$rho)
		
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

		tmp.PM <- update(obj$vam.PM$policy,Time[k]) # Peut-être ajout Vp comme argument de update


		k <- k+1

		# print(time.CM)
		# print(tmp.PM$time)

		if(time.CM < tmp.PM$time) {
			Time[k] <- time.CM
			Type[k] <- -1 #or -2, -3 dependening on the numbers of CM and the concurrency between CMs times
			mod2 <- obj$vam.CM[[1]]$model # 1 to be replaced with -Type[k]
		} else {
			Time[k] <- tmp.PM$time
			Type[k] <- tmp.PM$type
			mod2 <- obj$vam.PM$models[[Type[k]]]
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

