#-------------------
# TESTING logLik and AIC
#-------------------

library(sptotal)

#load the simulated data
data(simdata)

set.seed(1)

# take a new random sample of 300
obsID = sample(1:nrow(simdata), 300)
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
slmfit_out3 = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'REML',
  coordtype = "UTM")
slmfit_out4 = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Spherical", estmeth = 'REML',
  coordtype = "UTM")
slmfit_out5 = slmfit(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'REML',
  coordtype = "UTM")


summary(slmfit_out1)
GR2(slmfit_out1)
# compare models with different covariates using ML
logLik(slmfit_out1)
AIC(slmfit_out1)
logLik(slmfit_out2)
AIC(slmfit_out2)
# compare models with same covariates but different CorModel using REML
logLik(slmfit_out3)
AIC(slmfit_out3)
logLik(slmfit_out4)
AIC(slmfit_out4)

# check, does predict still work?
pout3 = predict(slmfit_out3)
pout3$FPBK_Prediction
pout3$PredVar
pout5 = predict(slmfit_out5)
pout5$FPBK_Prediction
pout5$PredVar
sum(simdata$Z)

library(splmm)
library(sp)
set.seed(3)
# take a new random sample of 100, just to make sure
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA
d2 = simobs
coordinates(d2) <- ~ x + y
slmfit_out6 = slmfit_jay(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'ML',
  coordtype = "UTM")
slmfit_out7 = slmfit(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmeth = 'ML',
  coordtype = "UTM")
slmfit_out8 = splmm(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  d2, estMeth = 'ML')
summary(slmfit_out6)
summary(slmfit_out7)
summary(slmfit_out8)

slmfit_out6$minus2loglik
slmfit_out7$minus2loglik
slmfit_out8$m2LL
# check, does predict still work?
pout6 = predict(slmfit_out6)
pout6$FPBK_Prediction
pout6$PredVar
pout7 = predict(slmfit_out7)
pout7$FPBK_Prediction
pout7$PredVar
sum(simdata$Z)

library(splmm)
d2 = d1
d2L = d2[d2$STRAT=='L',]
d2M = d2[d2$STRAT=='M',]
coordinates(d2) <- ~ x + y
coordinates(d2L) <- ~ x + y
coordinates(d2M) <- ~ x + y

splmmout1 = splmm(TOTAL ~ 1, d2) 
splmmout2 = splmm(TOTAL ~ STRAT, d2) 
splmmout3 = splmm(TOTAL ~ 1, d2L) 
splmmout4 = splmm(TOTAL ~ 1, d2M) 
