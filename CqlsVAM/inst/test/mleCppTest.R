#mleCppTest.R
require(CqlsVAM)
simCpp <- sim.vam.cpp(~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)) ))
simulate(simCpp,1000) -> simDf

mleCpp <- mle.vam.cpp(Time & Type ~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1000,prob=c(.5,.5)) ),data=simDf)

(run.mle.vam.cpp(mleCpp,c(1,2.5,.4,.7,.7)->par0)->res)