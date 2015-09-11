vam <- sim.vam(~(ARA1(.7) | Weibull(.1,2.5)) & (ARA1(.7) + ARA1(.6) | Periodic(10,prob=c(.5,.5)) ))
simulate(vam,1000) -> tmp
table(tmp$Type)