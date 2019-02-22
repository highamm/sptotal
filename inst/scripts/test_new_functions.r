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
slmfit_out1 = slmfit_jay(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'ML',
  coordtype = "UTM")
slmfit_out2 = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'ML',
  coordtype = "UTM")


summary(slmfit_out1)
summary(slmfit_out2)
GR2(slmfit_out1)
logLik(slmfit_out1)
AIC(slmfit_out1)
logLik(slmfit_out2)
AIC(slmfit_out2)
