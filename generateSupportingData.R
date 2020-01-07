# generate and save supporting datasets for nClimDiv shiny app
# MAC 12/12/2019

# libraries
library(maps)
library(ggplot2)
library(raster)
library(maptools)
library(tidyverse)

# ---- Geographic Data and lists ----
# FIPS codes
data(county.fips)
county.fips<-county.fips %>% 
  mutate(polyname = str_replace(polyname, "st", "saint"))

#save(county.fips, file = "countyFIPS.RData")
#load("countyFIPS.RData")

countyDf <- map_data('county')
countyList<-unique(countyDf$subregion)

# polygons for centroids
countiesPoly<-getData('GADM', country='USA', level=2)
statesPoly<-getData('GADM', country='USA', level=1)
# load cdiv shapefile
cdivPoly<-readShapePoly("climDivShp/GIS.OFFICIAL_CLIM_DIVISIONS")
#save(cdivPoly, file = "climDivShp.RData")
#load("climDivShp.RData")

# ggplot inset data
states4map <- map_data("state")

# state codes ----
stateCodes<-data.frame(code=c(seq(1,48,1),50,seq(101,110,1)),
                       name=c("Alabama","Arizona","Arkansas","California","Colorado",
                              "Connecticut","Delaware","Florida","Georgia","Idaho",
                              "Illinois","Indiana","Iowa","Kansas","Kentucky",
                              "Louisiana","Maine","Maryland","Massachusetts",
                              "Michigan","Minnesota","Mississippi","Missouri",
                              "Montana","Nebraska","Nevada","New Hampshire",
                              "New Jersey","New Mexico","New York","North Carolina",
                              "North Dakota","Ohio","Oklahoma","Oregon",
                              "Pennsylvania","Rhode Island","South Carolina",
                              "South Dakota","Tennessee","Texas","Utah",
                              "Vermont","Virginia","Washington","West Virginia",
                              "Wisconsin","Wyoming","Alaska","Northeast Region",
                              "East North Central Region","Central Region",
                              "Southeast Region","West North Central Region",
                              "South Region","Southwest Region",
                              "Northwest Region","West Region",
                              "National (contiguous 48 States)"
                       ))

# get boundary ----
us<-getData('GADM', country='USA', level=2)

# create list of counties, states, divs and regions ----
state.list<-statesPoly$NAME_1
state.list<-state.list[-which(state.list=="Alaska")]
state.list<-state.list[-which(state.list=="Hawaii")]
county.list<-countiesPoly@data[,c(4,7)]
county.list<-county.list[-which(county.list$NAME_1=="Alaska")]
county.list<-county.list[-which(county.list$NAME_1=="Hawaii")]
county.list<-paste0(county.list$NAME_1,"-",county.list$NAME_2)
cdiv.list<-cdivPoly@data[,c(2,7,10)]
cdiv.list<-paste0(cdiv.list$STATE,"-",cdiv.list$CD_NEW,", ",cdiv.list$NAME)

# create regions
load("./regions/combinedRegions.RData")

# updateSelectizeInput(session, 'x2', choices = list(
#   Eastern = c(`Rhode Island` = 'RI', `New Jersey` = 'NJ'),
#   Western = c(`Oregon` = 'OR', `Washington` = 'WA'),
#   Middle = list(Iowa = 'IA')
# ), selected = 'IA')

region.list<-list(
'Ag Region'=c("corn belt"="corn-belt","cotton belt"="cotton-belt","primary corn and soybean belt"="primary-corn-and-soybean-belt",
                "primary hard red winter wheat belt"="primary-hard-red-winter-wheat-belt","soybean belt"="soybean-belt",
                "spring wheat belt"="spring-wheat-belt", "winter wheat belt"="winter-wheat-belt"),
  'Climate Region'=c("Central"="central","Northeast"="northeast","Northern Rockies-Plains"="northern-rockies-plains",
                     "Northwest"="northwest","South"="south", "Southeast"="southeast","Southwest"="southwest",
                     "Upper Midwest"="upper-midwest","West"="west"),
  'NWS Region'=c("Central"="Central","Eastern"="Eastern","Southern"="Southern",
                 "Western"="Western"),
  'River Basin'=c("Arkansas-White-Red Basin"="arkansas-white-red-basin", "California River Basin"="california-basin",
                  "Great Lakes Basin"="great-lakes-basin", "Lower Colorado River Basin"="lower-co-basin","Lower Mississippi River Basin"="lower-ms-basin",
                  "Mid Atlantic Basin"="mid-atlantic-basin", "Missouri River Basin"="missouri-basin","New England Basin"="new-england-basin",
                  "Ohio River Basin"="ohio-basin","Pacific Northwest Basin"="pacific-nw-basin","Rio Grande River Basin"="rio-grande-basin",
                  "Souris-Red-Rainy Basin"="souris-red-rainy-basin","South Atlantic-Gulf Basin"="south-atlantic-basin","Tennessee River Basin"="tennessee-basin",
                  "Texas Gulf Coast River Basin"="texas-gulf-coast-basin","Upper Colorado River Basin"="upper-co-basin", 
                  "Upper Mississippi River Basin"="upper-ms-basin")
)

# attach codes to regions
regionCodes<-read.table("regionCodes.txt", header = TRUE, sep=",")
regionCodes$titles<-gsub("-", " ", regionCodes$name)

# save master file
save.image(file="nClimDivApp_Data.RData")

# maps https://www.ncdc.noaa.gov/monitoring-references/maps/
# 111 Great Plains
# 115 Southern Plains and Gulf Coast
# 120 US Rockies and Westward
# 121 Eastern Region
# 122 Southern Region
# 123 Central Region
# 124 Western Region
# 201 Pacific Northwest Basin
# 202 California River Basin
# 203 Great Basin
# 204 Lower Colorado River Basin
# 205 Upper Colorado River Basin
# 206 Rio Grande River Basin
# 207 Texas Gulf Coast River Basin
# 208 Arkansas-White-Red Basin
# 209 Lower Mississippi River Basin
# 210 Missouri River Basin
# 211 Souris-Red-Rainy Basin
# 212 Upper Mississippi River Basin
# 213 Great Lakes Basin
# 214 Tennessee River Basin
# 215 Ohio River Basin
# 216 South Atlantic-Gulf Basin
# 217 Mid-Atlantic Basin
# 218 New England Basin
# 220 Mississippi River Basin & Tributaties (N. of Memphis, TN)
# 
# (below codes are weighted by area)
# 250 Spring Wheat Belt
# 255 Primary Hard Red Winter Wheat Belt
# 256 Winter Wheat Belt
# 260 Primary Corn and Soybean Belt
# 261 Corn Belt
# 262 Soybean Belt
# 265 Cotton Belt
