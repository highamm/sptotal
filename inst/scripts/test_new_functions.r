#-------------------
# TRY MY CODE TO SPEED THINGS UP
#-------------------

library(sptotal)

#load the simulated data
data(simdata)

set.seed(1)

# take a new random sample of 300
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

#undebug(slmfit_jay)
#undebug(estcovparm_jay)
#undebug(m2LL_jay)
start = Sys.time()
slmfit_out = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'REML',
  coordtype = "UTM")
end = Sys.time()

summary(slmfit_out)
GR2(slmfit_out)
logLik(slmfit_out)
AIC(slmfit_out)
