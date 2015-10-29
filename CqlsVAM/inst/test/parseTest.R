require(CqlsVAM)
if(!("testExp" %in% ls(1))) testExp <- 1
formSim <- switch(testExp,
	~ (ARA1(.4) | Weibull(.001,2.5)),
	~ (ARA1(.8) | LogLinear(-5.0,0.5)),
	~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)) )
)

if(!("testExpMle" %in% ls(1))) testExpMle <- 1
formMle <- switch(testExpMle,
	Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)),
	System & Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)),
	Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)))
)

print(parse.vam.formula(NULL,formSim,Rcpp.mode=TRUE) ->modelSim)
print(parse.vam.formula(NULL,formMle,Rcpp.mode=TRUE)->modelMle)