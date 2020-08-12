##EpiModel simulations--template. I just ran a basic TB model on the Ambalatabaka village dataset.

library(statnet)

#Call up ambala_net network object
ambala_net <-readRDS("ambala_net.rds")


#Load EpiModel
library(EpiModel)

formation.ambala <-~edges+degree(0)+nodematch('role')+nodematch('sex')+nodematch('village')
target.stats.ambala <-c(197, 0, 64, 115, 179)
cbind(summary(simulate(ambala.10) ~ edges+degree(0)+nodematch('role')+nodematch('sex')+nodematch("village")))
coef.diss <-dissolution_coefs(dissolution = ~offset(edges), duration=20)


netest.ambala <-netest(ambala_net, formation.ambala, target.stats.ambala, coef.diss,
                     set.control.ergm=control.ergm(MCMC.burnin=1e5,
                                                   MCMC.interval=1000))


dx.ambala <-netdx(netest.ambala, nsims=10, nsteps=520)
dx.ambala
print(dx.ambala)
plot(dx.ambala, plots.joined = FALSE, qnts.alpha = 0.8)


#2) Run SEIR with no stochastic variables and no bespoke initialization module
source("/Users/mahazel/Desktop/SEIRModuleExtension.R")
source("/Users/mahazel/Box/Ashley/Remote/Active projects/Madagascar/ArrivalsAndDepartures.R")
param <-param.net(inf.prob=0.0004, act.rate=40, ei.rate=0.0074, ir.rate=0.0064, a.rate=0.00058,
                   ds.rate=0.00253, di.rate=0.0123)
init <-init.net(s.num=59, e.num=0, i.num=59, r.num=0) 
control <-control.net(type=NULL, nsteps=520, nsims=2, arrivals.FUN=arrivals.net, departures.FUN=departures.net, infection.FUN=infect2, progress.FUN = progress)
ambala_2 <-netsim(netest.ambala, param, init, control)

plot(tb.ambala_2, type="formation", main="TB spread")

par(mfrow = c(1,1), mar = c(4,4,4,4))
plot(tb.ambala_2,
     mean.col = 1:4, mean.lwd = 1, mean.smooth = TRUE,
     qnts = 1, qnts.col = 1:4, qnts.alpha = 0.5, qnts.smooth = FALSE,
     legend = TRUE)

