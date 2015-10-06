#mleCppTest.R
require(CqlsVAM)
require(CqlsPersistentRcppObject)

simulate(simCpp,1000) -> simDf
update(mleCpp,data=simDf)
switch(testExp,
(run.mle.vam.cpp(mleCpp,c(1,2.5,.4)->par0)->res),
(run.mle.vam.cpp(mleCpp,c(1,2.5,.4)->par0)->res),
(run.mle.vam.cpp(mleCpp,c(1,2.5,.4,.7,.7)->par0)->res),
(run.mle.vam.cpp(mleCpp,c(1,2.5,.4,.7,.7)->par0)->res)
)