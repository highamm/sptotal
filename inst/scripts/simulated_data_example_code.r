library(sptotal)

#load the simulated data
data(simdata)

set.seed(1)
# take a random sample of 100
obsID = sample(1:nrow(simdata), 100)
simobs = simdata
simobs$Z = NA
simobs[obsID,'Z'] = simdata[obsID,'Z']

# fit the model with all covariates
#undebug(slmfit)
#undebug(estcovparm)
simobs$Z
simobs$Z2 <- factor(simobs$Z)
slmfit_out1 = slmfit(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2,
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmethod = "None",
  covestimates = c(9, 4, 4))
# summary of fitted model
summary(slmfit_out1)
# function to get R^2
GR2(slmfit_out1)

# predictions for default total for whole population
class(slmfit_out1)
pred_info = predict(slmfit_out1)
FPBKoutput(pred_info, get_variogram = TRUE)

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

# predictions for mean over whole population
predout1 = predict(slmfit_out1, FPBKcol = 'wts1')
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
mean(simdata$Z)
# Is the truth in the prediction interval?
mean(simdata$Z) > PI['lower'] & mean(simdata$Z) < PI['upper']

# Simple random sampling estimate
SRS_est = mean(simobs$Z, na.rm = TRUE)
SRS_est
# Simple random sample standard error
SRS_var =
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
mean(simdata$Z)
# Is the truth in the prediction interval?
mean(simdata$Z) > SRSI['lower'] & mean(simdata$Z) < SRSI['upper']

# predictions for small area of 25 plots
predout1 = predict(slmfit_out1, FPBKcol = 'wts2')
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
Truth = sum(simdata[simdata$wts2 == 1,'Z'])
Truth
# Is the truth in the prediction interval?
Truth > PI['lower'] & Truth < PI['upper']

# Simple random sampling estimate
SRS_est = mean(simobs[simobs$wts2 == 1,'Z'], na.rm = TRUE)*
  sum(simobs$wts2 == 1)
SRS_est
# Simple random sample standard error
SRS_var = sum(simobs$wts2 == 1)^2*
  var(simobs[simobs$wts2 == 1,'Z'], na.rm = TRUE)/
  sum(!is.na(simobs[simobs$wts2 == 1,'Z']))*
  (1 - sum(!is.na(simobs[simobs$wts2 == 1,'Z']))/
    sum(simobs$wts2 == 1))
# standard error of SRS
sqrt(SRS_var)
# coefficient of variation of SRS
sqrt(SRS_var)/SRS_est
# 95% prediction interval
SRSI = c(lower = SRS_est - 1.96*sqrt(SRS_var),
  upper = SRS_est + 1.96*sqrt(SRS_var))
SRSI
# Truth
Truth = sum(simdata[simdata$wts2 == 1,'Z'])
Truth
# Is the truth in the prediction interval?
Truth > SRSI['lower'] & Truth < SRSI['upper']

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
slmfit_out2 = slmfit_jay(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2,
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
# DO IT AGAIN, SAMPLE SIZE OF 300
#-------------------

# take a new random sample of 300
obsID = sample(1:nrow(simdata), 300)
simobs = simdata
simobs[!(1:nrow(simdata) %in% obsID),'Z'] = NA

# fit the model with all covariates
#undebug(slmfit)
#undebug(estcovparm)
slmfit_out3 = slmfit_jay(Z ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2,
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
slmfit_out4$AICvalue

#X1 does not appear to be significant, so fit another model without it.
slmfit_out5 = slmfit(Z ~ X2 + X3 + X4 + X5 + X6 + X7 + F1 + F2,
  data = simobs, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential", estmethod = 'ML',
  coordtype = "UTM")
# summary of fitted model
summary(slmfit_out5)
slmfit_out5$AICvalue

#-------------------
# TRY MY CODE TO SPEED THINGS UP
#-------------------

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
