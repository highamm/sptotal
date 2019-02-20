library(sptotal)

#create systematic grid of points
xy = pointSimSyst(nrow = 30, ncol = 30, lower_x_lim = 0, upper_x_lim = 1, 
    lower_y_lim = 0, upper_y_lim = 1) 
    
plot(xy)

set.seed(1)

# create autocorrelated random error structure
eps = geostatSim(xy, xcol = "x", ycol = "y", 
	parsil = 1, range = 1, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]

#create independent continuous X1 variable
X1 = rnorm(nrow(xyz))
#create independent continuous X2 variable
X2 = rnorm(nrow(xyz))
#create independent continuous X3 variable
X3 = rnorm(nrow(xyz))
#create independent continuous X4 variable
X4 = rnorm(nrow(xyz))
#create independent continuous X5 variable
X5 = rnorm(nrow(xyz))
#create autocorrelated continuous X6 variable
X6 = geostatSim(xy, xcol = "x", ycol = "y", 
	parsil = 1, range = .2, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]
#create autocorrelated continuous X7 variable
X7 = geostatSim(xy, xcol = "x", ycol = "y", 
	parsil = 1, range = .2, nugget = .01,
	minorp = 1, rotate = 90, extrap = NULL,
	CorModel = "Exponential")[,3]
#create factor F1 variable
F1 = as.factor(rep(1:3, times = nrow(xyz)/3))
F1 = F1[order(runif(nrow(xyz)))]
#create factor F2 variable
F2 = as.factor(rep(1:5, times = nrow(xyz)/5))
F2 = F2[order(runif(nrow(xyz)))]

Z = 10 + 0*X1 + .1*X2 + .2*X3 + .3*X4 + .4*X5 + .4*X6 + .1*X7 + 
  .4*(as.numeric(F1) - 2) + .1*(as.numeric(F2) - 3) + 2*eps
  
simdata = data.frame(x = xy$x, y = xy$y, X1 = X1, X2 = X2, X3 = X3, X4 = X4,
  X5 = X5, X6 = X6, X7 = X7, F1 = F1, F2 = F2, Z = Z)
obsID = sample(1:nrow(xyz), 100)
simobs = simdata
simobs[!(1:nrow(xyz) %in% obsID),'Z'] = NA

# fit a model where STRAT is a covariate
#undebug(slmfit)
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

#default map
plot(predout1)

#change a few parameters for plotting predictions
plot(predout1, nbreaks = 20, breakMethod = 'even', cex = 2,
  legend.cex = 1.3)

# The total estimate of moose,
predout1$FPBK_Prediction
# and its standard error
sqrt(predout1$PredVar)
# yielding a coefficient of variation (CV)
sqrt(predout1$PredVar)/predout1$FPBK_Prediction

# Try a model without stratification
slmfit_out2 = slmfit(TOTAL ~ 1, data = d1, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")

# predictions
predout2 = FPBKpred(slmfit_out2)
# The total estimate of moose,
predout2$FPBK_Prediction
# and its standard error
sqrt(predout2$PredVar)
# yielding a coefficient of variation (CV)
sqrt(predout2$PredVar)/predout2$FPBK_Prediction

# Do estimates by strata
slmfit_outL = slmfit(TOTAL ~ 1, data = d1[d1$STRAT == 'L',], xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")
slmfit_outM = slmfit(TOTAL ~ 1, data = d1[d1$STRAT == 'M',], xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")

# prediction
predoutL = FPBKpred(slmfit_outL)
predoutM = FPBKpred(slmfit_outM)
# The total estimate of moose,
predoutL$FPBK_Prediction + predoutM$FPBK_Prediction
# and its standard error
sqrt(predoutL$PredVar +predoutM$PredVar)
# yielding a coefficient of variation (CV)
sqrt(predoutL$PredVar +predoutM$PredVar)/
  (predoutL$FPBK_Prediction + predoutM$FPBK_Prediction)


mean(d1[d1$STRAT=='L',]$TOTAL, na.rm = TRUE)
mean(d1[d1$STRAT=='M',]$TOTAL, na.rm = TRUE)

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

slmfit_out1$SpatialParmEsts
splmmout1$theta
slmfit_out2$SpatialParmEsts
splmmout2$theta
slmfit_out3$SpatialParmEsts
splmmout3$theta
slmfit_out4$SpatialParmEsts
splmmout4$theta
