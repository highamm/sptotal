library(sptotal)

#load the simulated data
data(USlakes)
# look at distribution of data
hist(log(USlakes$DOC_RESULT))
# plot locations
plot(USlakes[,'XCOORD'], USlakes[,'YCOORD'])

# make a graph showing outliers on map
png('/media/jay/ExtraDrive1/sptotal_pack/sptotal/inst/scripts/lakes.png')
plot(USlakes$XCOORD, USlakes$YCOORD)
points(USlakes[USlakes$DOC_RESULT > 200,'XCOORD'],
  USlakes[USlakes$DOC_RESULT > 200,'YCOORD'], col = 'red', pch = 19)
dev.off()

# create a data set to work with
lakes = USlakes
lakes = lakes[lakes$DOC_RESULT < 200,]
lakes = lakes[!(lakes$YCOORD < -1000000 & lakes$XCOORD < 0),]
hist(lakes$DOC_RESULT)
plot(lakes$XCOORD, lakes$YCOORD)

# always make scatter plots of response versus covariates
# try tranformations of covariates too
plot((lakes$ELEVATION+min(lakes$ELEVATION))^.33, log(lakes$DOC_RESULT))
plot(lakes$RVFPUNDWOODY_RIP, log(lakes$DOC_RESULT))
# what proportion are exactly 0
sum(lakes$FCIBIG_LIT == 0)/nrow(lakes)
plot((lakes$FCIBIG_LIT)^.33, log(lakes$DOC_RESULT))
# what proportion are exactly 0
sum(lakes$RVFCGNDBARE_RIP == 0)/nrow(lakes)
plot((lakes$RVFCGNDBARE_RIP)^.33, log(lakes$DOC_RESULT))
# what proportion are exactly 0
sum(lakes$RVFCGNDWOODY_RIP == 0)/nrow(lakes)
plot((lakes$RVFCGNDWOODY_RIP)^.33, log(lakes$DOC_RESULT))
# check for colinearity
cor(lakes[,2:6])
# check for matching zeros
sum(lakes$FCIBIG_LIT == 0 & lakes$RVFCGNDBARE_RIP == 0)/nrow(lakes)
# create dummy variable for covariates with lots of zeros that are
# included in a continuous variable
lakes$FCIBIG_gt0 = (lakes$FCIBIG_LIT != 0)*1
lakes$RVFCGNDBARE_gt0 = (lakes$RVFCGNDBARE_RIP != 0)*1
lakes$RVFCGNDWOODY_gt0 = (lakes$RVFCGNDWOODY_RIP != 0)*1

# try classical regression without log of DOC_RESULT
# note use of binary variable for covariates with 0's
# and use of interaction with non-zero values
lmout = lm(DOC_RESULT ~ I((lakes$ELEVATION+min(lakes$ELEVATION))^.33) +
  RVFPUNDWOODY_RIP +
  FCIBIG_gt0 + FCIBIG_gt0:I(FCIBIG_LIT^.33) +
  RVFCGNDBARE_gt0 + RVFCGNDBARE_gt0:I(RVFCGNDBARE_RIP^.33) +
  RVFCGNDWOODY_gt0 + RVFCGNDWOODY_gt0:I(RVFCGNDWOODY_RIP^.33),
  data = lakes)
summary(lmout)
hist(residuals(lmout))

# Even though data are skewed, let's try it without taking
# log of response variable. Note that the mean of log-tranformed
# variables is not equal to the log of the mean of set of variables.
# So if we want a total on the untransformed scale, it would be a
# mistake to transform the data first, model it, make predictions,
# sum the predictions, and then exponentiate.  It is much simpler
# to leave the data untransformed and rely on robustness of the
# method.
set.seed(1)
# take a random sample of 100
obsID = sample(1:nrow(lakes), 100)
lakes_samp = lakes
lakes_samp[!(1:nrow(simdata) %in% obsID),'DOC_RESULT'] = NA

# fit the model with all covariates
slmfit_out1 = slmfit(
  DOC_RESULT ~ ##I((lakes$ELEVATION+min(lakes$ELEVATION))^.33) +
    RVFPUNDWOODY_RIP +
  FCIBIG_gt0, ##+ FCIBIG_gt0:I(FCIBIG_LIT^.33) +
 ## RVFCGNDBARE_gt0 + RVFCGNDBARE_gt0:I(RVFCGNDBARE_RIP^.33) +
 ## RVFCGNDWOODY_gt0 + RVFCGNDWOODY_gt0:I(RVFCGNDWOODY_RIP^.33),
  data = lakes_samp, xcoordcol = 'XCOORD', ycoordcol = 'YCOORD',
  CorModel = "Exponential")
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
sum(lakes$DOC_RESULT)
# Is the truth in the prediction interval?
sum(lakes$DOC_RESULT) > PI['lower'] & sum(lakes$DOC_RESULT) < PI['upper']

# Simple random sampling estimate of total
SRS_est = mean(lakes_samp$DOC_RESULT, na.rm = TRUE)*nrow(lakes)
SRS_est
# Simple random sample standard error
SRS_var = length(lakes_samp$DOC_RESULT)^2*
  var(lakes_samp$DOC_RESULT, na.rm = TRUE)/
  sum(!is.na(lakes_samp$DOC_RESULT))*
  (1 - sum(!is.na(lakes_samp$DOC_RESULT))/length(lakes_samp$DOC_RESULT))

# standard error of SRS
sqrt(SRS_var)
# coefficient of variation of SRS
sqrt(SRS_var)/SRS_est
# 95% prediction interval
SRSI = c(lower = SRS_est - 1.96*sqrt(SRS_var),
  upper = SRS_est + 1.96*sqrt(SRS_var))
SRSI
# Truth
sum(lakes$DOC_RESULT)
# Is the truth in the prediction interval?
sum(lakes$DOC_RESULT) > SRSI['lower'] & sum(lakes$DOC_RESULT) < SRSI['upper']

# make the default graph
plot(predout1)

# could resample from the true population many times, in a loop,
# to test for unbiasedness, root-mean-squared prediction errors (RMSPE),
# and prediction interval coverage
