require(CqlsVAM)
testExp <- 1
formSim <- switch(testExp,
	~ (ARA1(.4) | Weibull(.001,2.5)),
	~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)) )
)

formMle <- switch(testExp,
	Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)),
	Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)))
)

parse.vam.formula(NULL,formSim,Rcpp.mode=TRUE) ->modelSim
parse.vam.formula(NULL,formMle,Rcpp.mode=TRUE)->modelMle