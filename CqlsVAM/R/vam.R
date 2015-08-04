# Virtual Age Models (VAM)

# Simulation: sim.vam or vam.sim or vam.gen??? 

# data is here just in case you want to complete data (TODO later)
sim.vam <- function(formula,data) {
	# Have to generate both C + P
	# formula has to provide the two parts
	# sim.vam(~ Weibull(ARA1(2) + ARA1(3,periodic(100))))
	# TODO: parsing formula later

	obj <- 	list(
				vams=list(ara1.va.model(.5),ara1.va.model(.6,maintenance.policy.periodic(100))),
				family=weibull.family.vam(1.2)
			)
	class(obj) <- "sim.vam"
	obj
}

# stop.policy: number of events, events before time,....
simulate.sim.vam <- function(obj,nbsim=1,stop.policy = Inf,seed = NULL) {
	# return a data.frame of the form
	# data.frame(Time=,Type=)
	

}

# Estimation part! The usual way in R

vam <- function(formula,data) {

}

summary.vam <- function(obj,...) {

}

