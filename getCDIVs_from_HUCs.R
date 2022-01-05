# watersheds and climate divisions
# MAC 05/20/21

library(rgdal)

ogrListLayers("~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb")
HUC4 = readOGR("~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb","WBDHU4")

# load cdiv shapefile
cdivPoly<-maptools::readShapePoly("/home/crimmins/RProjects/ClimPlot/spiPlots/climDivShp/GIS.OFFICIAL_CLIM_DIVISIONS")
# ggplot inset data
statesPoly<-raster::getData('GADM', country='USA', level=1)


# subset to single watershed
subHUC4<-subset(HUC4, HUC4=="0204")

plot(cdivPoly, xlim=c(-80,-70), ylim=c(35,45))
plot(subHUC4, add=TRUE, c="red")

# intersect of HUC and CDIV
intHUC<-raster::intersect(subHUC4, cdivPoly)
  percArea<-raster::area(intHUC)
  percArea<-(percArea/sum(percArea)*100)
  #colnames(percArea)<-"area"
  idx <- order(percArea)
  cumPerc<-cumsum(percArea[idx])[order(idx)]
  intHUC<-intHUC[which(cumPerc>10),]
  
intHUC<-subset(cdivPoly, CLIMDIV %in% unique(intHUC$CLIMDIV))

# which CDIVs?
cdivList<-unique(intHUC$CLIMDIV)

