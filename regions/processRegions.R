# process NOAA nClimDiv regions
# MAC 12/12/2019

# need to add name of region to each shapefile then combine

library(raster)

# regionList - climate regions have diff CRS
regionList<-c("./regions/ag-regions",
              "./regions/river-basins")
regionCode<-c("Ag Region",
              "River Basin")

datalist = list()
counter=1

for(j in 1:length(regionList)){
  # ag regions
  shps <- dir(regionList[j], "*.shp", full.names = TRUE)
  shps<-shps[-grep(".xml", shps)]
  regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps))
  # loop through shapes in regions
    for(i in 1:length(shps)){
      tempShp<-rgdal::readOGR(shps[i])
      tempShp<-tempShp[,-(1:ncol(tempShp))]
      tempShp$region<-regionName[i]
      tempShp$region2<-regionCode[j]
      datalist[[counter]] <- tempShp
      counter<-counter+1
    }
}
combinedRegions = do.call(rbind, datalist)

# do climate regions
regionList<-c("./regions/climate-regions")
regionCode<-c("Climate Region")
j=1
datalist = list()
counter=1
shps <- dir(regionList[j], "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps))
# loop through shapes in regions
for(i in 1:length(shps)){
  tempShp<-rgdal::readOGR(shps[i])
  tempShp<-tempShp[,-(1:ncol(tempShp))]
  tempShp$region<-regionName[i]
  tempShp$region2<-regionCode[j]
  datalist[[counter]] <- tempShp
  counter<-counter+1
}
combinedRegions2 = do.call(rbind, datalist)

# transform to common CRS
combinedRegions2<-spTransform(combinedRegions2, crs(combinedRegions))
# then combine
combinedRegions<-bind(combinedRegions, combinedRegions2)

# load NWS regions
shps <- dir("./regions/nws-regions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
nwsRegions<-do.call(rbind, lapply(shps[1], rgdal::readOGR))
  colnames(nwsRegions@data)[1]<-"region"
  nwsRegions$region2<-"NWS Region"    
    
# combine again
combinedRegions<-bind(combinedRegions,nwsRegions)

save(combinedRegions, file="./regions/combinedRegions.RData")

# add division codes to data.frame

# test regions
subArea<-subset(combinedRegions,region=="Western")
centroid<-coordinates(subArea)

# river basins
shps <- dir("./regions/river-basins", "*.shp", full.names = TRUE)
  shps<-shps[-grep(".xml", shps)]
  riverBasins<-do.call(rbind, lapply(shps[1], rgdal::readOGR))

  # river basins
shps <- dir("./regions/climate-regions", "*.shp", full.names = TRUE)
  shps<-shps[-grep(".xml", shps)]
  climateRegions<-do.call(rbind, lapply(shps[1], rgdal::readOGR))
  
  # river basins
  shps <- dir("./regions/ag-regions", "*.shp", full.names = TRUE)
  shps<-shps[-grep(".xml", shps)]
  agRegions<-do.call(rbind, lapply(shps[1], rgdal::readOGR))
  
# NWS regions 
  shps <- dir("./regions/nws-regions", "*.shp", full.names = TRUE)
  shps<-shps[-grep(".xml", shps)]
  nwsRegions<-do.call(rbind, lapply(shps[1], rgdal::readOGR))
  
  
