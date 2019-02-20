# TODO: Add comment
# 
# Author: Paco
###############################################################################

library(sptotal)
library(sp)
library(rgeos)
library(rgdal)

# use Oregon biomass data
data(ORbiomass)
# plot data, which are in points in Oregon  Lambert Conformal Conic projection
plot(ORbiomass)
ORbiomass@proj4string
ORbiomass_df<-slot(cbind(ORbiomass,coordinates(ORbiomass)),"data")
#http://spatialreference.org/ref/epsg/nad83harn-oregon-lambert/

sampled_rows<-sample(dim(ORbiomass_df)[1],1000)
ORbiomass_df_subsample<-ORbiomass_df
ORbiomass_df_subsample[c(1:dim(ORbiomass_df)[1])%in%sampled_rows,]$BIOT_ha<-NA
slmfit_OR = slmfit(BIOT_ha ~ temp_mean_30yr + precip_mean_30yr,
		data = ORbiomass_df_subsample, xcoordcol = 'LON', ycoordcol = 'LAT',
		CorModel = "Exponential",
		coordtype = "UTM")
# summary of fitted model
summary(slmfit_OR)
# function to get R^2
GR2(slmfit_OR)

# predictions
predOR = FPBKpred(slmfit_OR)

#default map
plot(predOR)

#change a few parameters for plotting predictions
plot(predOR, nbreaks = 20, breakMethod = 'even', cex = 2,
		legend.cex = 1.3)

# The total estimate of moose,
predOR$FPBK_Prediction
# and its standard error
sqrt(predOR$PredVar)
# yielding a coefficient of variation (CV)
sqrt(predOR$PredVar)/predOR$FPBK_Prediction

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


