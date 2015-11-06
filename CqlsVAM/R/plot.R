plot.model.vam.cpp <- function(obj,type=c("virtual.age","intensity","cumulative","h","H"),from,to,by=0.1) {
	rcpp <- rcpp(obj)
	d <- rcpp$get_selected_data(0) #0 since one-system first!
	infos <- rcpp$get_virtual_age_infos(by)
	if(missing(from)) from <- min(d$Time)
	if(missing(to)) to <- max(d$Time)
	type <- match.arg(type)
	switch(type,
		virtual.age={
			var <- "v"
			ylab<-"virtual age"
		},
		intensity=,h={
			var <- "h"
			ylab<-"intensity"
		},
		cumulative=,H={
			var <- "H"
			ylab<-"cummulative"
		}
	)
	ymax<-max(unlist(sapply(infos,function(e) e[[var]])))
	plot(c(from,to),c(0,ymax),xlim=c(from,to),xlab="time",ylab=ylab,type="n")
	
	# if(FALSE) for(i in seq_along(infos)) {
	# 	info<-infos[[i]]
	# 	lines(info$t,info[[var]])
	# }

	if(TRUE) {
		t <- infos[[1]]$t
		v <- infos[[1]][[var]]
		for(i in seq_along(infos)[-1]) {
			t<-c(t,NA,infos[[i]]$t)
			v <- c(v,NA,infos[[i]][[var]])
		}
		lines(t,v)
	}

}