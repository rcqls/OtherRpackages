#mleCppTest.R
require(CqlsVAM)
require(CqlsPersistentRcppObject)

nExp<-rep(10,1000)
#nExp <-10000
cat("Simulating...\n")
simulate(simCpp,nExp,as.list=length(nExp)>1) -> simDf
## simDf <- rbind(cbind(System=1,simDf),cbind(System=2,simDf))
##print("toto")
##print(data.frame.to.list.mle.vam.cpp(simDf,c("System","Time","Type")))
##
cat("Table of number of system:\n")
print(table(nExp))
mleTmp <- if(length(nExp)>1) mleCppMulti else mleCpp
update(mleTmp,data=simDf)

print(coef(mleTmp,
	switch(testExp,
		c(1,2.5,.5),
 		c(1,2.5,.5),
 		c(1,2.5,.5,.5,.5),
 		c(1,2.5,.5,.5,.5)
 	)->par0)->res
)