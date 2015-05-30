# Virtual Age Models (VAM)

# Simulation 

# data is here just in case you want to complete data (TODO later)
sim.vam <- function(formula,family="weibull",data) {
	# Have to generate both MC + MP
	# formula has to provide the two parts
	# sim.vam(~ARA1(rho=2) + ARA1(rho=3,P="") )
	# TODO parsing formula later

	obj <- list(vam=ara.va.model(1,rhoC=.5,1,rhoP=.6),family=weibull.family.vam())
	class(obj) <- "sim.vam"
	obj
}

# stop.policy: number of events, events before time,....
simulate.sim.vam <- function(obj,nbsim=1,stop.policy=10,seed = NULL) {
	# return a data.frame of the form
	# data.frame(Time=,Type=)

}

# Estimation part! The usual way in R

vam <- function(formula,data,family="weibull") {

}

summary.vam <- function(obj,...) {

}

