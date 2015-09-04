# Virtual Age Models (VAM)
vam <- function(formula,data) {
	# simulation: 		vam(~ (ARA1(.4) | Weibull(1,2.5)) & (ARA1(.7) + ARAInf(.6) | Periodic(100,prob=c(.5,.5)) ) )
	# mle estimation:	vam(Time & Type ~ (ARA1(.4) | Weibull(1,2.5)) & (ARA1(.7) + ARAInf(.6) | Periodic(100,prob=c(.5,.5)) ) )
}

# Simulation: sim.vam or vam.sim or vam.gen??? 

# data is here just in case you want to complete data (TODO later)
sim.vam <- function(formula,data) {
	# sim.vam(~ (ARA1(.4) | Weibull(1,2.5)) & (ARA1(.7) + ARAInf(.6) | Periodic(100,prob=c(.5,.5)) ) )

	obj <- new.env()
	parse.sim.vam.formula(obj,formula)

	# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????

	class(obj) <- "sim.vam"
	obj
}

parse.sim.vam.formula <- function(obj,formula) {
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
			# TODO: add obj as argument of policy when needed
			policy <- eval(policy)
			# PMs
			pm <- pm[[2]]
			# parser for pm
			parse.pm <- function(pm) {
				if(is.name(pm[[1]])) {
					pm[[1]] <- as.name(paste0(as.character(pm[[1]]),".va.model"))
				}
				pm[[3]] <- as.name("obj")
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
		cm[[length(cm)+1]] <- as.name("obj")
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

	
	# update the object with CMs and PMs
	obj$vam.CM <- cms[rev(seq(cms))]
	obj$vam.PM <- list(models=pms[rev(seq(pms))],policy=policy)
}

init.sim.vam <- function(obj) {
	obj$cache<-list(Vp=0,k=1,mod=obj$vam.PM$models[[1]]) # 1 because of current obj$Type
	obj$Time <- 0 
	obj$Type <- 1
}

# stop.policy: number of events, events before time,....
simulate.sim.vam <- function(obj, nbsim=10, stop.time = Inf,seed = NULL) {
	# return a data.frame of the form
	# data.frame(Time=,Type=)
	
	init.sim.vam(obj)

	# stop.policy
	if(stop.time != Inf) {
		nbsim <- Inf
		stop.policy <- list(type="event",time=stop.time)
	} else {
		stop.policy<-list(type="counter",size=nbsim)
	}

	# when concurrency between CMs occurs this line would be inside the loop
	family <- obj$vam.CM[[1]]$family

	while(obj$cache$k < nbsim && obj$Time[obj$cache$k] < stop.time) {
		### modAV <- if(Type[k]<0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[obj$Type[k]]]

		time.CM <- inverse.virtual.age(obj$cache$mod,inverse.cummulative.density(family,cummulative.density(family,virtual.age(obj$cache$mod,obj$Time[obj$cache$k]))-log(runif(1))))

		timeAndType.PM <- update(obj$vam.PM$policy,obj$Time[obj$cache$k]) # Peut-être ajout Vp comme argument de update


		# 
		obj$cache$k <- obj$cache$k+1

		if(time.CM < timeAndType.PM$time) {
			obj$Time[obj$cache$k] <- time.CM
			obj$Type[obj$cache$k] <- -1 #or -2, -3 dependening on the numbers of CM and the concurrency between CMs times
			# new model!
			mod <- obj$vam.CM[[1]]$model # 1 to be replaced with -Type[k]
		} else {
			obj$Time[obj$cache$k] <- timeAndType.PM$time
			obj$Type[obj$cache$k] <- timeAndType.PM$type
			# new model!
			mod <- obj$vam.PM$models[[obj$Type[obj$cache$k]]]
		}

		# update new model (i.e. mod) with respect to the old one (obj$cache$mod)!
		update(mod)
		#obj$cache$Vp <- mod$Vp(obj$cache$mod$V(Time[k],k,Time,Vp,mod$rho),Vp,mod2$rho)
		# save current model
		obj$cache$mod <- mod
	}

	res <- data.frame(Time=obj$Time[-1],Type=obj$Type[-1])
	attr(res,"stop.policy") <- stop.policy
	res
}

# Estimation part! The usual way in R

mle.vam <- function(formula,data) {
	# Have to generate both C + P
	# formula has to provide the two parts
	# vam( Time & Type ~ (ARA1(rhoCM) | Weibull(alpha,beta)) & (ARA1(rhoPM[1]) + ARAInf(rhoPM[2]) | Periodic(100,prob=c(.5,.5)) ) , data= df)

	obj <- new.env()
	parse.sim.vam.formula(obj,formula)

	# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????

	class(obj) <- "mle.vam"
	obj

}

# param: alpha,beta,rhoCM,rhoPM[]
constrat.mle.vam <- function(obj,param) {

}

summary.mle.vam <- function(obj,...) {

}

