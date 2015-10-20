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
	obj$cache <- list()
	obj$response <- parse.vam.formula(obj,formula)
	
	# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????

	attr(obj,"formula") <- formula
	class(obj) <- "sim.vam"
	obj
}

init.sim.vam <- function(obj) {
	obj$cache$Vright<-0
	obj$cache$k<-1
	obj$cache$mod<-obj$vam.PM$models[[1]] # 1 because of current obj$data$data$Type
}

init.data.vam <- function(obj) {
	if(is.null(obj$response)) {
		obj$data <- data.frame(Time=0,Type=1)
	} else { ## TODO!!!
		# to complete data: 
		# 		-> first, estimate with parameters values used as initialization
		#		-> then simulate!
		obj$data <- data.frame(Time=data[[obj$response[1]]],Type=data[[obj$response[2]]])
		obj$cache$k <- NCOL(obj$data)
	} 
} 


# stop.policy: number of events, events before time,....
simulate.sim.vam <- function(obj, nbsim=10, stop.time = Inf,seed = NULL) {
	# return a data.frame of the form
	# data.frame(Time=,Type=)
	
	init.sim.vam(obj)
	init.data.vam(obj)

	# stop.policy
	if(stop.time != Inf) {
		nbsim <- Inf
		stop.policy <- list(type="event",time=stop.time)
	} else {
		stop.policy<-list(type="counter",size=nbsim)
	}

	# when concurrency between CMs occurs this line would be inside the loop
	family <- obj$vam.CM[[1]]$family

	while(obj$cache$k < nbsim && obj$data$Time[obj$cache$k] < stop.time) {
		### modAV <- if(Type[k]<0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[obj$data$Type[k]]]
		# Here, obj$cache$k means k-1
		#print(c(obj$cache$Vleft,obj$cache$Vright))
		time.CM <- inverse.virtual.age(obj$cache$mod,inverse.cummulative.density(family,cummulative.density(family,virtual.age(obj$cache$mod,obj$data$Time[obj$cache$k]))-log(runif(1))))

		timeAndType.PM <- update(obj$vam.PM$policy,obj$data$Time[obj$cache$k]) # Peut-être ajout Vright comme argument de update
#print(c(time.CM,timeAndType.PM$time))
		if(time.CM < timeAndType.PM$time) {
			obj$data <- rbind(obj$data, c(time.CM,-1)) #or -2, -3 dependening on the numbers of CM and the concurrency between CMs times
			# new model!
			mod <- obj$vam.CM[[1]]$model # 1 to be replaced with -Type[k]
		} else {
			obj$data <- rbind(obj$data,c(timeAndType.PM$time,timeAndType.PM$type))
			# new model!
			mod <- obj$vam.PM$models[[obj$data$Type[obj$cache$k+1]]]
		}
		# used in the next update
		update.Vleft.vam(obj)
		# update the next k, and save model in cache too!
		update(mod)
	}

	res <- data.frame(Time=obj$data$Time[-1],Type=obj$data$Type[-1])
	attr(res,"stop.policy") <- stop.policy
	res
}

sim.vam.cpp <- function(formula,data) {
	if("package:CqlsPersistentRcppObject" %in% search()) {
		self <- newEnv(sim.vam.cpp,formula=formula)

		PersistentRcppObject(self,new = {
			model <- parse.vam.formula(NULL,self$formula,Rcpp.mode=TRUE)
			rcpp <- new(SimVamCpp,model)
			rcpp 
		})

		self
	} else {
		obj <- new.env()
		model <- parse.vam.formula(NULL,formula,Rcpp.mode=TRUE)
		obj$rcpp <- new(SimVamCpp,model)
		attr(obj,"formula") <- formula
		class(obj) <- "sim.vam.cpp"
		obj
	}
}

simulate.sim.vam.cpp <- function(self, n=10, stop.time = Inf,as.list=FALSE) {
	rcpp <- if("package:CqlsPersistentRcppObject" %in% search()) self$rcpp() else self$rcpp
	if(length(n)>1) {
		# multisystem
		if(as.list) df<-list()
		for(i in seq_along(n)) {
			df2 <- rcpp$simulate(n[i])[-1,]
			if(as.list) {
				df[[i]] <- df2 #rbind(data.frame(Time=0,Type=1),df2)
			} else {
				df2$System <- i
				df2<-df2[c(3,1:2)]
				df <- if(i==1) df2 else rbind(df,df2)
			}
		}
	} else df <- rcpp$simulate(n)[-1,]
	if(!as.list) rownames(df) <- 1:nrow(df)
	df
}

# Estimation part! The usual way in R

mle.vam <- function(formula,data) {
	# Have to generate both C + P
	# formula has to provide the two parts
	# vam( Time & Type ~ (ARA1(rhoCM) | Weibull(alpha,beta)) & (ARA1(rhoPM[1]) + ARAInf(rhoPM[2]) | Periodic(100,prob=c(.5,.5)) ) , data= df)

	obj <- new.env()
	obj$cache <- list()
	response <- parse.vam.formula(obj,formula)
	if(is.null(response)) stop("Left part of formula is required!")
	else {
		obj$response <- response
		obj$data <- data.frame(Time=c(0,data[[response[1]]]),Type=c(1,data[[response[2]]]))
	}
	# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????

	attr(obj,"formula") <- formula
	class(obj) <- "mle.vam"
	obj

}

init.mle.vam <- function(obj,with.gradient=FALSE) {
	obj$cache$Vright <- 0 #Inf	# useless value at init
	obj$cache$k<- 1 # different from simulation
	obj$cache$mod <- obj$vam.PM$models[[1]] # useless value at init
	# the rest
	obj$cache$S1 <- 0
	obj$cache$S2 <- 0
	obj$cache$S3 <- sum(obj$data$Type<0)
	obj$cache$hVleft <- 0
	if(with.gradient) {
		obj$cache$dVright <- rep(0,1+length(obj$vam.PM)) #not with respect to beta
		obj$cache$dS1 <- rep(0,2+length(obj$vam.PM))
		obj$cache$dS2 <- rep(0,2+length(obj$vam.PM))
	}
}

contrast.update.mle.vam <- function(obj,with.gradient=FALSE) {
	update.Vleft.vam(obj,with.gradient)
	obj$cache$S1 <- obj$cache$S1 + (cummulative.density(obj$vam.CM[[1]]$family,obj$cache$Vleft)) - (cummulative.density(obj$vam.CM[[1]]$family,obj$cache$Vright))
	#if(is.nan(obj$cache$hVleft) || obj$cache$hVleft<=0) print(list("hVleft",obj$cache$hVleft))
	obj$cache$S2 <- obj$cache$S2 + log(obj$cache$hVleft <- density(obj$vam.CM[[1]]$family,obj$cache$Vleft))*((obj$data$Type[obj$cache$k+1]<0)->obj$cache$indCM)
}

gradient.update.mle.vam <- function(obj,with.gradient=TRUE) {
	#cat("1111\n");print(obj$cache$dS1)
	contrast.update.mle.vam(obj,with.gradient)
	#cat("2222\n");print(obj$cache$dS1)
	#print(list("1",cummulative.density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft), cummulative.density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vright)))
	obj$cache$dS1 <- obj$cache$dS1 + c(
			cummulative.density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft) - cummulative.density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vright),
			obj$cache$hVleft * obj$cache$dVleft - density(obj$vam.CM[[1]]$family,obj$cache$Vright) * obj$cache$dVright
		)
	#cat("3333\n");print(obj$cache$dS1)
	#print(list("1",density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft),obj$cache$hVleft,obj$cache$indCM))
	#print(list("2",density.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft), obj$cache$dVleft))
	obj$cache$dS2 <- obj$cache$dS2 + c(
			density.param.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft)/obj$cache$hVleft * obj$cache$indCM,
			density.derivative(obj$vam.CM[[1]]$family,obj$cache$Vleft) * obj$cache$dVleft/obj$cache$hVleft * obj$cache$indCM
		)
}

params.sim.vam <- params.mle.vam <- function(obj,param) {
	if(missing(param)) {
		c(params(obj$vam.CM[[1]]$family),params(obj$vam.CM[[1]]$model),sapply(obj$vam.PM$models,params))
	} else {
		params(obj$vam.CM[[1]]$family,param[1:2])
		params(obj$vam.CM[[1]]$model,param[3])
		for(i in seq(obj$vam.PM$models)) params(obj$vam.PM$models[[i]],param[3+i])
	}
}

# params: beta,rhoCM,rhoPM[]
contrast.mle.vam <- function(obj,param) {
	init.mle.vam(obj)
	params(obj,param)
	while(obj$cache$k < nrow(obj$data)) {
		contrast.update.mle.vam(obj)
		# previous model for the next step
		Type <- obj$data$Type[obj$cache$k+1]
		#print(Type)
		mod <- if(Type < 0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[Type]]
		update(mod)
	}
	# log-likelihood (at constant)
	#print(list(param,-log(obj$cache$S1) * obj$cache$S3 + obj$cache$S2))
	-log(obj$cache$S1) * obj$cache$S3 + obj$cache$S2
}

gradient.mle.vam <- function(obj,param) {
	init.mle.vam(obj,with.gradient=TRUE)
	params(obj,param)
	while(obj$cache$k < nrow(obj$data)) {
		#if(obj$cache$k ==1) print(list("before",obj$cache))
		gradient.update.mle.vam(obj)
		#if(obj$cache$k ==1) print(list("after",obj$cache))
		# previous model for the next step
		Type <- obj$data$Type[obj$cache$k+1]
		mod <- if(Type < 0) obj$vam.CM[[1]]$model else obj$vam.PM$models[[Type]]
		update(mod,with.gradient=TRUE)
	}
	# return gradient
	#print(list(param,-obj$cache$dS1/obj$cache$S1 * obj$cache$S3 + obj$cache$dS2))
	-obj$cache$dS1/obj$cache$S1 * obj$cache$S3 + obj$cache$dS2
}

summary.mle.vam <- function(obj,...) {

}

run.mle.vam<-function(obj,par0,fixed,method=NULL,verbose=TRUE,...) {
	## parameters stuff!
	if(missing(par0))  {
		if("par" %in% names(obj)) param <- obj$par #not the first run 
		else param<-params(obj) #first run
	} else if(is.null(par0)) param<-params(obj) else param<-par0
	## fixed and functions stuff!
	if(missing(fixed)) fixed<-rep(FALSE,length(param))
	else if(is.numeric(fixed)) {
		fixedInd<-fixed
		fixed<-rep(FALSE,length(param))
		fixed[fixedInd]<-TRUE
	}

	fn<-function(par) {
		##print(par);print(param[!fixed])
		param[!fixed]<-par
		##cat("param->");print(param)
		-contrast.mle.vam(obj,param)
	}

 
	gr <- function(par) {
	    param[!fixed]<-par
	    -c(0,gradient.mle.vam(obj,param))[!fixed]
	}
  
  ## optim stuff!
  if(is.null(method) || method=="fast") {
    if(length(param[!fixed])>1) param[!fixed]<-(res <- optim(param[!fixed],fn,gr,method="Ne",...))$par
    if(is.null(method)) res<-optim(param[!fixed],fn,gr,method="CG",...)
  } else res<-optim(param[!fixed],fn,gr,method=method,...)
  
  #fixed tips
  param[!fixed]<-res$par
  res$par<-param
  
  if(verbose) print(res)

  ## save stuff
  obj$fixed <- fixed
  obj$optim<-res
  obj$par<-res$par
  res$par
}

update.Vleft.vam <- function(obj,with.gradient=FALSE) {
	obj$cache$Vleft <- virtual.age(obj$cache$mod,obj$data$Time[obj$cache$k+1])
	#print(obj$cache$Vleft)
	if(with.gradient) obj$cache$dVleft <- virtual.age.derivative(obj$cache$mod,obj$data$Time[obj$cache$k+1])
} 


mle.vam.cpp <- function(formula,data) {
	if("package:CqlsPersistentRcppObject" %in% search()) {
		self <- newEnv(mle.vam.cpp,formula=formula,data=data)

		PersistentRcppObject(self,new = {
			model <- parse.vam.formula(NULL,self$formula,Rcpp.mode=TRUE)
			response <- model$response
			data <- data.frame.to.list.mle.vam.cpp(self$data,response)
			rcpp <- new(MLEVamCpp,model,data)
			rcpp 
		})

		self
	} else {
		obj <- new.env()
		model <- parse.vam.formula(NULL,formula,Rcpp.mode=TRUE)
		response <- model$response
		data <- data.frame(Time=c(0,data[[response[1]]]),Type=c(1,data[[response[2]]]))
		# todo: scale to take from Weibull and replace alpha with 1 in the weibull ????
		obj$rcpp <- new(MLEVamCpp,model,data)
		attr(obj,"formula") <- formula
		class(obj) <- "mle.vam.cpp"
		obj
	}
}

data.frame.to.list.mle.vam.cpp <- function(data,response) {
	# return data if it is already only a list!
	if(is.list(data) && !is.data.frame(data)) return(lapply(data,function(df) rbind(data.frame(Time=0,Type=1),df)))
	# otherwise
	if(length(response)==2) {
		if(length(intersect(response,names(data))) != 2) stop(paste0("Bad response:",response))
		tmp <- data[[response[1]]]
		data2 <- list(data.frame(Time=c(0,tmp[order(tmp)]),Type=c(1,data[[response[2]]][order(tmp)])))
	} else {
		if(length(intersect(response,names(data))) != 3) stop(paste0("Bad response:",response))
		syst0 <- unique(syst<-data[[response[1]]])
		data2 <- list()
		for(i in seq_along(syst0)) {
			df <- data[syst==syst0[i],response]
			tmp <- df[[response[2]]]
			data2[[i]] <- data.frame(Time=c(0,tmp[order(tmp)]),Type=c(1,df[[response[3]]][order(tmp)]))
		}
	}
	data2
}

# to convert in Rcpp



params.sim.vam.cpp <- params.mle.vam.cpp <- function(self,param) {
	if(missing(param)) {
		 self$rcpp()$get_params()
	} else {
		self$rcpp()$set_params(param)
	}
}

update.mle.vam.cpp <- function(self,data) {
	if(!missing(data)) {
		model <- parse.vam.formula(NULL,self$formula,Rcpp.mode=TRUE)
		response <- model$response
		self$data <- data
		data2 <- data.frame.to.list.mle.vam.cpp(self$data,response)
		self$rcpp()$set_data(data2)
	}
}

# alpha is not considered in the estimation!
run.mle.vam.cpp<-function(obj,par0,fixed,method=NULL,verbose=TRUE,...) {
	rcpp <- if("package:CqlsPersistentRcppObject" %in% search()) obj$rcpp() else obj$rcpp

	## parameters stuff!
	if(missing(par0))  {
		if("par" %in% names(obj)) param <- obj$par #not the first run 
		else param<-params(obj)[-1] #first run
	} else if(is.null(par0)) param<-params(obj)[-1] else param<-par0[-1]
	## fixed and functions stuff!
	if(missing(fixed)) fixed<-rep(FALSE,length(param))
	else if(is.numeric(fixed)) {
		fixedInd<-fixed
		fixed<-rep(FALSE,length(param))
		fixed[fixedInd]<-TRUE
	}

	fn<-function(par) {
		##print(par);print(param[!fixed])
		param[!fixed]<-par
		##cat("param->");print(param)
		-rcpp$contrast(c(1,param))
	}

 
	gr <- function(par) {
	    param[!fixed]<-par
	    -rcpp$gradient(c(1,param))[!fixed]
	}
  
  ## optim stuff!
  if(is.null(method) || method=="fast") {
    if(length(param[!fixed])>1) param[!fixed]<-(res <- optim(param[!fixed],fn,gr,method="Ne",...))$par
    if(is.null(method)) res<-optim(param[!fixed],fn,gr,method="CG",...)
  } else res<-optim(param[!fixed],fn,gr,method=method,...)
  
  #fixed tips
  param[!fixed]<-res$par
  res$par<-param
  
  if(verbose) print(res)

  ## save stuff
  obj$fixed <- fixed
  obj$optim<-res
  obj$par<-res$par
  res$par
}

coef.mle.vam.cpp <- function(obj,par) {
	res <-run.mle.vam.cpp(obj,par,verbose=FALSE)
	if(obj$optim$convergence>0) cat("convergence=",obj$optim$convergence,"\n",sep="")
	alpha <- obj$rcpp()$alpha_est(c(1,res))
	c(alpha,res)
}

# for both sim and mle

parse.vam.formula <- function(obj,formula,Rcpp.mode=FALSE) {
	if(formula[[1]] != as.name("~")) stop("Argument has to be a formula")
	if(length(formula) == 2) {
		response <- NULL
		cm <- formula[[2]]
	} else {
		tmp <- formula[[2]]
		if(tmp[[1]] != as.name("&") && length(tmp) != 3) stop("Left part of formula of the form 'Time & Type'!")
		if(length(tmp[[2]])==3 && tmp[[2]][[1]]==as.name("&")) {
			response <- c(as.character(tmp[[2]][[2]]),as.character(tmp[[2]][[3]]),as.character(tmp[[3]]))
		} else response <- c(as.character(tmp[[2]]),as.character(tmp[[3]]))
		cm <- formula[[3]]
	}
	pms <- list()
	policy <- NULL
	if(cm[[1]] == as.name("&")) { # there is a PM part
		pm <- cm[[3]]
		cm <- cm[[2]]
		# deal with PM part
		if(pm[[1]] == as.name("(")) {
			pm <- pm[[2]]
			if(pm[[1]] != as.name("|")) stop("Need a policy to manage Preventive Maintenance")
			policy <- pm[[3]]
			if(is.name(policy[[1]])) {
				policy[[1]] <- as.name(paste0(as.character(policy[[1]]),".maintenance.policy"))
			}
			# TODO: add obj as argument of policy when needed
			if(!Rcpp.mode) policy <- eval(policy)
			# PMs
			pm <- pm[[2]]
			# parser for pm
			parse.pm <- function(pm) {
				if(is.name(pm[[1]])) {
					pm[[1]] <- as.name(paste0(as.character(pm[[1]]),".va.model"))
				}
				pm[[3]] <- as.name("obj")
				if(Rcpp.mode) pm else eval(pm)
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
		if(Rcpp.mode) list(model=cm,family=family) else list(model=eval(cm),family=eval(family))
	}
	cpt.cms <- 0
	while( cm[[1]] == as.name("+") ) {
		if(length(cm) == 3) {
			cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm[[3]])
			cm <- cm[[2]]
		}
	}
	cms[[cpt.cms <- cpt.cms + 1]] <- parse.cm(cm)

	if(Rcpp.mode) {
		convert.cm <- function(cm) {

			list(
				model=list(
					name=as.character(cm$model[[1]]),
					params=sapply(cm$model[2:(length(cm$model)-1)],as.vector)
				),
				family=list(
					name=as.character(cm$family[[1]]),
					params=sapply(cm$family[-1],as.vector)
				)
			)
		}
		convert.pm <- function(pm) {
			list(
				name=as.character(pm[[1]]),
				params=sapply(pm[2:(length(pm)-1)],as.vector)
			)

		}
		convert.mp <- function(mp) {#maintenance policy
			if(is.null(mp)) list(name="None") 
			else { 
				pars=as.list(match.call(eval(mp[[1]]),mp))[-1]
				# default values!
				if(is.null(pars[["from"]])) pars["from"]<-0
				if(is.null(pars[["prob"]])) pars["prob"]<-1	
				list(
					name=as.character(mp[[1]]),
					params=lapply(pars,eval)
				)
			}
		}
		cms <- convert.cm(cms[[1]])
		list(
			response=response,
			models=c(list(cms$model),lapply(pms[rev(seq(pms))],convert.pm)),
			family=cms$family,
			pm.policy=convert.mp(policy)
		)
	
	} else { 
	
		# update the object with CMs and PMs
		obj$vam.CM <- cms[rev(seq(cms))]
		obj$vam.PM <- list(models=pms[rev(seq(pms))],policy=policy)
		# for matching purpose
		obj$vam.CM[[1]]$model$id <-0
		for(i in seq(obj$vam.PM$models)) obj$vam.PM$models[[i]]$id <- i 
		# return response
		response # NULL for sim
	} 
}



