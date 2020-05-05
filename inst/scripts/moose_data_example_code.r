library(sptotal)
library(sp)
library(rgeos)
library(rgdal)

# use Alaska Moose data
data(AKmoose)
# plot data, which are in polygons in Alaska Albers projection
plot(AKmoose)

AKmoose@proj4string
gCentroid(AKmoose,byid=TRUE)@coords[,'x']
# see http://spatialreference.org/ref/epsg/nad83-alaska-albers/

# transform the projection to lat/lon
latlon = spTransform(AKmoose, CRS("+init=epsg:4326"))
plot(latlon)
latlon@proj4string
# see http://spatialreference.org/ref/epsg/4326/

#get centroids of polygons
centroids = data.frame(ID = latlon@data,
  x = gCentroid(latlon,byid=TRUE)@coords[,'x'],
  y = gCentroid(latlon,byid=TRUE)@coords[,'y'])

# create user-defined UTM coordinates for spatial modeling
xy = LLtoUTM(mean(centroids$x), centroids$y, centroids$x)$xy

# pull the data.frame from the SpatialPolygonsDataFrame object
AKmoose@plotOrder
d1 = AKmoose@data
rownames(d1)
str(d1)
# add new UTM coordinates to data.frame,
d1$x = xy[,'x']
d1$y = xy[,'y']
# check to make sure that ordering didn't get messed up
# by comparing to lat lon in the data frame
str(d1)
d1[1:30, c('CENTRLAT', 'y', 'CENTRLON','x')]
d1$totalnum <- as.numeric(levels(d1$total))[d1$total]
d1$sampind <- as.numeric(levels(d1$surveyed))[d1$surveyed]
d1$totalnum[d1$sampind == 0] <- NA
# fit a model where STRAT is a covariate
slmfit_out1 = slmfit(totalnum ~ strat, data = d1, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential")
# summary of fitted model
summary(slmfit_out1)
# function to get R^2
GR2(slmfit_out1)
residuals(slmfit_out1)
coef(slmfit_out1)

# predictions
predout1 = predict(slmfit_out1)
predout1$FPBK_Prediction / 0.84
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
