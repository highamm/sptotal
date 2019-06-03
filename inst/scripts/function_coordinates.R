# TODO: Add comment
# 
# Author: Paco
###############################################################################
library(sp)
library(rgdal)

sptotal_datacheck<-function(data,xcoordcol, ycoordcol){
	
#	I would put this first check to see if data is a data.frame
#	in the function slmfit, so we don't have to pass the data frame to another function,
#	R is going to make a copy of the passed data frame if it is large then is a waste 
#	of memory.
	if(inherits(data,"data.frame")){
		
		return(data)
		
	}
	
#	allows to pass the path of a shapefile and loads it
	
	if(is.character(data)){
			
		data<-try({
						
				geo_object<-sub('\\.shp$', '',basename(data))				
				geo_folder<-dirname(data)
				readOGR(geo_folder,geo_object)
						
				})
		
		if(class(data)=="try-error"){
			# If the path is wrong or there is an error loading the sp object returns 
			# data that will be of class "try-error"
			return(data)
			
		}
	
	}	
		
#	If it is not a character checks if it is a spatial object
#
#	Checks if the data has a reference system. 
#	
#	If the data does not have spatial reference raises one or two warnings. 
#		The first one is always raised and informs the used that the spatial metadata (Reference system is missing)
#		The second warning is raised if after checking the x coordinates and y coordinates they
#		have values that can correspond to limits 
#		for lat-lon coordinates (i.e. x in [-180,360],y[-90,90])
#	
#	If it has a reference system and it is a projected system returns the 
#	data.frame of the object with the spatial coordinates added to xcoordcol and ycoordcol
#	
#	If it has a reference system and it is not projected (geographic coordinates lat-lon)
#	stops and rasises an error printing a message saying that the data has to be projected

	
	if(inherits(data,"Spatial")){
		
		projected<-is.projected(data)
		
		if(is.na(projected)){
			
			data@data[,c(xcoordcol,ycoordcol)]<-coordinates(data)
			data<-slot(data,"data")
			
			message<-"Spatial data without spatial reference metadata,\n"
			message<-paste(message,"If you loaded a shapefile using a string with the shapefile path\n",sep="")
			message<-paste(message,"the '*.prj' file might be missing",sep="")
			warning(message,call.=FALSE)
			
			if(max(data[,xcoordcol])<360 & min(data[,xcoordcol])< -180 & 
					max(abs(data[,ycoordcol]))<90){
				
				message<-"Based on range of the x and y coordinates they seem to be lat-long\n"
				message<-paste(message,"Distances computed from latitudes and longitude are not meaningful\n")
				message<-paste(message,"Project your data")
				warning(message,call.=FALSE)
				
			}
			
			return(data)

				
		}else{
				
			if(projected){			
				
				data@data[,c(xcoordcol,ycoordcol)]<-coordinates(data)
				data<-slot(data,"data")
				return(data)
			}else{
				message<-"The coordinates of data are lat-long\n"
				message<-paste(message,"Distances computed from latitudes and longitude are not meaningful\n",sep="")
				message<-paste(message,"Project your data", sep="")
				stop(message,call.=FALSE)
			}
		
		}
			
	}

#	If it is not a not a data.frame, character ( with a path) or spatial object	

	stop("Data is not a data.frame, character (path) or spatial object")

}
