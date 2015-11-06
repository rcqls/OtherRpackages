require(CqlsVAM)
require(CqlsPersistentRcppObject)
if(!("testExp" %in% ls(1)))  testExp <- 1

formSim <- switch(testExp,
	~ (ARA1(.4) | Weibull(.001,2.5)),
	~ (ARAInf(.4) | Weibull(.001,2.5)),
	~ (ARA1(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(1,prob=c(.5,.5)) ),
	~ (ARAInf(.4) | Weibull(.001,2.5)) & (ARA1(.7) + ARA1(.7) | Periodic(10,prob=c(.5,.5)) ),
	~ (ARA1(.8) | LogLinear(exp(-5),0.5))
)

formModel <- update(formSim,Time & Type ~ .)
formMle <- formModel
formModelMulti <- update(formSim,System & Time & Type ~ .)
formMleMulti <- formModelMulti

simCpp <- sim.vam.cpp(formSim)

nExp <- 1000
simulate(simCpp,nExp) -> simDf


modelCpp <- model.vam.cpp( formModel ,data=simDf)


mleCpp <- mle.vam.cpp( formMle ,data=simDf)


nExp <- rep(1000,2)
simulate(simCpp,nExp) -> simDf
mleCppMulti <- mle.vam.cpp( formMleMulti ,data=simDf)

# mleCpp <- list()
# for(i in 1:10) {
# 	simulate(simCpp,1000) -> simDf
# 	mleCpp[[i]] <- mle.vam.cpp( formMle ,data=simDf)
# }
# for(i in 1:10) {
# 	simulate(simCpp,1000) -> simDf
# 	update_data.mle.vam.cpp(mleCpp[[i]],simDf)
# }
# for(i in 1:10) {
# 	simulate(simCpp,1000) -> simDf
# 	update_data.mle.vam.cpp(mleCpp[[i]],simDf)
# }