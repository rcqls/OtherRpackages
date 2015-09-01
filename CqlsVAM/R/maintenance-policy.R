## prob expresses the vector of probability as in the sample function 
## The values need not sum to 1!
## Other strategy to fix the type of the event could be investigated (by cycling for example)

Periodic.maintenance.policy <- function(by,from=0,prob=1) {
	obj <- list(from=from,by=by,prob=prob)
	class(obj) <- c("Periodic.maintenance.policy","maintenance.policy")
	obj 
}

update.Periodic.maintenance.policy <- function(obj,current) {
	list(
		time=obj$from + (floor((current-obj$from)/obj$by) + 1 ) * obj$by,
		type=sample.int(length(obj$prob),1,replace=TRUE,prob=obj$prob)
	)
}

Recycling.maintenance.policy <- function(inter.times,types) {
	if(length(inter.times) != length(types)) stop("inter.times and types have to have the same length")
	obj <- new.env()
	obj$inter.times <- inter.times
	obj$types <- types
	obj$ntypes <- length(types)
	obj$current.time <- inter.times[1]
	obj$current.index <- types[1]

	class(obj) <- c("Recycling.maintenance.policy","maintenance.policy")
	obj 
}

## recycling policy: recycling along a sequence of events and types

update.Recycling.maintenance.policy <- function(obj,current) {
	if(current > obj$current.time) {
		obj$current.index <- (obj$current.index %% obj$ntypes) + 1
		obj$current.time <- obj$current.time + obj$inter.times[obj$current.index]
	}
	list(time=obj$current.time,type=obj$types[obj$current.index]) 
}


## times and types are prefixed. Inf is added at the end to ensure the end of maintenance

Fixed.maintenance.policy <- function(times,types) {
	obj <- new.env()
	obj$times <- c(times,Inf)
	obj$types <- c(types,NA)
	obj$current.index <- 1
	class(obj) <- c("Fixed.maintenance.policy","maintenance.policy")
	obj 
}

update.Fixed.maintenance.policy <- function(obj,current) {
	if(current > obj$times[obj$current.index]) obj$current.index <- obj$current.index + 1
	list(time=obj$times[obj$current.index],type=obj$types[obj$current.index]) 
}

