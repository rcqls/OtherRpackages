#mleCppTest.R
require(CqlsVAM)
require(CqlsPersistentRcppObject)

nExp <-10000
cat("Simulating...\n")
simulate(simCpp,nExp,as.list=length(nExp)>1) -> simDf
cat("Number of events:",nExp,"\n")
update(mleCpp,data=simDf)

print(coef(mleCpp,
	switch(testExp,
		c(1,2.5,.5),
 		c(1,2.5,.5),
 		c(1,2.5,.5,.5,.5),
 		c(1,2.5,.5,.5,.5),
 		c(0,1,.5),
 	)->par0)->res
)