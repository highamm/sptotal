library(sptotal)

#load the simulated data
data(simdata)

set.seed(1)
# take a random sample of 100
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# fit the model with all covariates
#undebug(slmfit) 
#undebug(estcovparm)
slmfit_out1 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out1)
# function to get R^2
GR2(slmfit_out1)

# predictions
predout1 = predict(slmfit_out1)
predout1$FPBK_Prediction
sqrt(predout1$PredVar)
# Truth
sum(simdata$Z)

#-------------------
# DO IT AGAIN
#-------------------

# take a new random sample of 100
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA


# fit the model with all covariates
#undebug(slmfit) 
#undebug(estcovparm)
slmfit_out1 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out1)
# function to get R^2
GR2(slmfit_out1)

# predictions
predout1 = predict(slmfit_out1)
predout1$FPBK_Prediction
sqrt(predout1$PredVar)
# Truth
sum(simdata$Z)


#-------------------
# DO IT AGAIN
#-------------------

# take a new random sample of 500
obsID = sample(1:nrow(simdata), 500)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA


# fit the model with all covariates
#undebug(slmfit) 
#undebug(estcovparm)
slmfit_out1 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out1)
# function to get R^2
GR2(slmfit_out1)

# predictions
predout1 = predict(slmfit_out1)
predout1$FPBK_Prediction
sqrt(predout1$PredVar)
# Truth
sum(simdata$Z)
