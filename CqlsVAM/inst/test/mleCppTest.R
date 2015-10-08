#mleCppTest.R
require(CqlsVAM)
require(CqlsPersistentRcppObject)

nExp<-100000
simulate(simCpp,nExp) -> simDf
update(mleCpp,data=simDf)

print(coef(mleCpp,
	switch(testExp,
		c(1,2.5,.4),
 		c(1,1,.9),
 		c(1,2.5,.4,.7,.7),
 		c(1,2.5,.4,.7,.7)
 	)->par0)->res
)