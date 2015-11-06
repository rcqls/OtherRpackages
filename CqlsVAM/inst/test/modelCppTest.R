require(CqlsVAM)
require(CqlsPersistentRcppObject)
nExp <-20
cat("Simulating...\n")
simulate(simCpp,nExp,as.list=length(nExp)>1) -> simDf
cat("Number of system:",nExp,"\n")
update(modelCpp,data=simDf)
plot(modelCpp)