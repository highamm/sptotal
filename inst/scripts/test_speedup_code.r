#-------------------
# TRY MY CODE TO SPEED THINGS UP
#-------------------

library(sptotal)

#load the simulated data
data(simdata)

set.seed(1)

# take a new random sample of 300
obsID = sample(1:nrow(simdata), 300)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# try it with my code
#X1 does not appear to be significant, so fit another model without it.
#undebug(slmfit_jay)
#undebug(estcovparm_jay)
#undebug(m2LL_jay)
start5 = Sys.time()
slmfit_out5 = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'REML',
  coordtype = "UTM")
end5 = Sys.time()
start6 = Sys.time()
slmfit_out6 = slmfit(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'REML',
  coordtype = "UTM")
end6 = Sys.time()
summary(slmfit_out5)
summary(slmfit_out6)
end5 - start5
end6 - start6
