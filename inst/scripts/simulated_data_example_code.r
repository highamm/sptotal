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
# prediction of total
predout1$FPBK_Prediction
# standard error of prediction
sqrt(predout1$PredVar)
# coefficient of variation of prediction
sqrt(predout1$PredVar)/predout1$FPBK_Prediction
# 95% prediction interval
PI = c(lower = predout1$FPBK_Prediction - 1.96*sqrt(predout1$PredVar),
  upper = predout1$FPBK_Prediction + 1.96*sqrt(predout1$PredVar))
PI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > PI['lower'] & sum(simdata$Z) < PI['upper']

# Simple random sampling estimate
SRS_est = mean(simobs$Z, na.rm = TRUE)*length(simobs$Z)
SRS_est
# Simple random sample standard error
SRS_var = length(simobs$Z)^2*
  var(simobs$Z, na.rm = TRUE)/
  sum(!is.na(simobs$Z))*
  (1 - sum(!is.na(simobs$Z))/length(simobs$Z))
# standard error of SRS
sqrt(SRS_var)
# coefficient of variation of SRS
sqrt(SRS_var)/SRS_est
# 95% prediction interval
SRSI = c(lower = SRS_est - 1.96*sqrt(SRS_var),
  upper = SRS_est + 1.96*sqrt(SRS_var))
SRSI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > SRSI['lower'] & sum(simdata$Z) < SRSI['upper']

#-------------------
# DO IT AGAIN  WITH A DIFFERENT SAMPLE
#-------------------

# take a new random sample of 100
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# fit the model with all covariates
#undebug(slmfit) 
#undebug(estcovparm)
slmfit_out2 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out2)
# function to get R^2
GR2(slmfit_out2)

# predictions
predout2 = predict(slmfit_out2)
# prediction of total
predout2$FPBK_Prediction
# standard error of prediction
sqrt(predout2$PredVar)
# coefficient of variation of prediction
sqrt(predout2$PredVar)/predout2$FPBK_Prediction
# 95% prediction interval
PI = c(lower = predout2$FPBK_Prediction - 1.96*sqrt(predout2$PredVar),
  upper = predout2$FPBK_Prediction + 1.96*sqrt(predout2$PredVar))
PI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > PI['lower'] & sum(simdata$Z) < PI['upper']

# Simple random sampling estimate
SRS_est = mean(simobs$Z, na.rm = TRUE)*length(simobs$Z)
SRS_est
# Simple random sample standard error
SRS_var = length(simobs$Z)^2*
  var(simobs$Z, na.rm = TRUE)/
  sum(!is.na(simobs$Z))*
  (1 - sum(!is.na(simobs$Z))/length(simobs$Z))
# standard error of SRS
sqrt(SRS_var)
# coefficient of variation of SRS
sqrt(SRS_var)/SRS_est
# 95% prediction interval
SRSI = c(lower = SRS_est - 1.96*sqrt(SRS_var),
  upper = SRS_est + 1.96*sqrt(SRS_var))
SRSI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > SRSI['lower'] & sum(simdata$Z) < SRSI['upper']

#-------------------
# DO IT AGAIN, SAMPLE SIZE OF 500
#-------------------

# take a new random sample of 500
obsID = sample(1:nrow(simdata), 500)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# fit the model with all covariates
#undebug(slmfit) 
#undebug(estcovparm)
slmfit_out3 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out3)
# function to get R^2
GR2(slmfit_out3)

# predictions
predout3 = predict(slmfit_out3)
# prediction of total
predout3$FPBK_Prediction
# standard error of prediction
sqrt(predout3$PredVar)
# coefficient of variation of prediction
sqrt(predout3$PredVar)/predout3$FPBK_Prediction
# 95% prediction interval
PI = c(lower = predout3$FPBK_Prediction - 1.96*sqrt(predout3$PredVar),
  upper = predout3$FPBK_Prediction + 1.96*sqrt(predout3$PredVar))
PI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > PI['lower'] & sum(simdata$Z) < PI['upper']

# Simple random sampling estimate
SRS_est = mean(simobs$Z, na.rm = TRUE)*length(simobs$Z)
SRS_est
# Simple random sample standard error
SRS_var = length(simobs$Z)^2*
  var(simobs$Z, na.rm = TRUE)/
  sum(!is.na(simobs$Z))*
  (1 - sum(!is.na(simobs$Z))/length(simobs$Z))
# standard error of SRS
sqrt(SRS_var)
# coefficient of variation of SRS
sqrt(SRS_var)/SRS_est
# 95% prediction interval
SRSI = c(lower = SRS_est - 1.96*sqrt(SRS_var),
  upper = SRS_est + 1.96*sqrt(SRS_var))
SRSI
# Truth
sum(simdata$Z)
# Is the truth in the prediction interval?
sum(simdata$Z) > SRSI['lower'] & sum(simdata$Z) < SRSI['upper']


#-------------------
# TRY MAXIMUM LIKELIHOOD, RATHER THAN REML
#-------------------

# take a new random sample of 100
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# fit the model with all covariates
slmfit_out4 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmethod = 'ML',
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out4)

#X1 does not appear to be significant, so fit another model without it.
slmfit_out5 = slmfit(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2, 
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmethod = 'ML',
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out5)
