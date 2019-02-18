library(sptotal)
library(sp)
library(rgeos)
library(rgdal)

data(AKmoose)

latlon = spTransform(AKmoose, CRS("+init=epsg:4326"))
plot(latlon)

#get centroids
centroids = data.frame(ID = latlon@data$ID, 
  x = gCentroid(latlon,byid=TRUE)@coords[,'x'],
  y = gCentroid(latlon,byid=TRUE)@coords[,'y'])

xy = LLtoUTM(mean(centroids$x), centroids$y, centroids$x)$xy

d1 = AKmoose@data
str(d1)
d1$x = xy[,'x']
d1$y = xy[,'y']

#undebug(slmfit)
slmfit_out = slmfit(TOTAL ~ STRAT, data = d1, xcoordcol = 'x', ycoordcol = 'y',
  CorModel = "Exponential",
  coordtype = "UTM")

predout1 = FPBKpred(slmfit_out)

#default map
plot(predout1)

#change a few parameters
plot(predout1, nbreaks = 20, breakMethod = 'even', cex = 2,
  legend.cex = 1.4)

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
