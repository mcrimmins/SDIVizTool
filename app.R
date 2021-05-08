# SPI and SPEI plots using nClimDiv data
# MAC 12/7/19
#

# To do: latest date update in selector, plotly heatmap colors don't match, plotly legend names

# load shiny libraries
library(shiny)
library(shinythemes)

# load code libraries
library(reshape2)
library(RCurl)
#library(maps)
library(raster)
#library(ggplot2)
library(cowplot)
library(tidyverse)
#library(zoo)
#library(maptools)
library(SPEI)
library(weathermetrics)
#library(metR)
library(scales)
library(magick)
library(plotly)


# load datasets
# ---- Functions ----
# capitalize county names
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
# custom date picker
dateRangeInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}
# add/subtracting months
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
# ------

# load supporting data from Rdata file generateSupportingData.R
load("nClimDivApp_Data.RData")

# date list for picker - advances on 10th of month
sysDay<-as.numeric(format(Sys.Date(),"%d"))
if(sysDay<=9){
    date.list<-seq(as.Date("1895/1/1"), add.months(Sys.Date(),-2), "months")
}else{
    date.list<-seq(as.Date("1895/1/1"), add.months(Sys.Date(),-1), "months")
}
latest.date<-max(date.list)
# ----


# UI code

ui <- fluidPage(theme=shinytheme('sandstone'),
  
    tags$head(HTML(
                  "<!-- Global site tag (gtag.js) - Google Analytics -->
                  <script async src='https://www.googletagmanager.com/gtag/js?id=UA-155656786-1'></script>
                  <script>
                    window.dataLayer = window.dataLayer || [];
                    function gtag(){dataLayer.push(arguments);}
                    gtag('js', new Date());
                  
                    gtag('config', 'UA-155656786-1');
                  </script>")),
                
  titlePanel(strong("Standardized Drought Index Visualization Tool")),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("selectOpt", "Select spatial scale",
                   list("Climate Division"='dv', "County"='cy',"State"='st',"Region"='rg'),
                   selected = 'dv'),
      conditionalPanel(
        condition = "input.selectOpt=='dv'",
        selectizeInput(
          inputId = "sel_dv", 
          label = "Select a division",
          choices = cdiv.list,
          selected=cdiv.list[27]
        )),
      conditionalPanel(
        condition = "input.selectOpt=='cy'",
        selectizeInput(
          inputId = "sel_cy", 
          label = "Select a county",
          choices = county.list,
          selected = county.list[104]
        )),
      conditionalPanel(
        condition = "input.selectOpt=='st'",
        selectizeInput(
          inputId = "sel_st", 
          label = "Select a state",
          choices = state.list,
          selected = state.list[2]
        )),
      conditionalPanel(
        condition = "input.selectOpt=='rg'",
        selectizeInput(
          inputId = "sel_rg", 
          label = "Select a region",
          choices = region.list,
          selected = region.list[[1]][1]
        )),
      dateRangeInput2("dateRangeMY", "Select date range for plot (1895-present)", startview = "year", minview = "months", maxview = "decades",
                      start = "1981-01-01", end = latest.date, min = min(date.list), max = latest.date),
      sliderInput("maxTimescale", "Max timescale to display (y-axis):",
                  min = 3, max = 60, value = 60),
      radioButtons("petOpt", "Select potential evapotranspiration (PET) estimation method for SPEI",
                   list("Hargreaves (recommended)"='harg', "Thornthwaite"='thornW'),
                   selected = 'harg'),
      actionButton("updateAll","Update"),
      hr(),
      HTML('<div style="text-align: center;">Contact Mike Crimmins (<a
                               href="mailto:crimmins@email.arizona.edu">crimmins@email.arizona.edu</a>)
                               with questions or comments. SDI Viz Tool v1.0 01/07/20</div>'
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("SPI", 
                 #img(src="spiPlot.png", width="50%", align="left"),
                  plotOutput("spiImage", width = "100%"),
                  tags$head(tags$style(type="text/css", "#spiImage img { border: 1; max-width: 100%; }
                                                           element.style { width: 33.33%; }"))),
        tabPanel("SPEI", 
                 #img(src="spiPlot.png", width="50%", align="left"),
                 plotOutput("speiImage", width = "100%"),
                 tags$head(tags$style(type="text/css", "#speiImage img { border: 1; max-width: 100%; }
                                                           element.style { width: 33.33%; }"))),        
        tabPanel("Interactive SPI & SPEI", 
                 br(),
                 p("Hover cursor over plots to interrogate values. Use plot image controls to zoom in/out, pan, reset axes, and download snapshot."),
                 br(),
                 plotlyOutput('SPIPlotly', width = "auto"),
                 br(),
                 plotlyOutput('SPEIPlotly',  width = "auto"),
                 br(),
                 p("This plot shows the difference between SPI and SPEI values for each month and timescale. Purple colors indicate when 
                   SPI values were more positive than SPEI and orange colors vice versa. For example, a difference value of 1 could
                   indicate that the SPEI (-1) is more negative than the SPI (0) reflecting more intense drought conditions."),
                 plotlyOutput('diffPlotly',  width = "auto")
                 ), 
        tabPanel("Explore Monthly Data", 
                 br(),
                 p("All of the monthly nClimDiv data used in the calculation of the SPI and SPEI plots are displayed in the plots this page.
                   The first plot shows the long-term (1895-present) monthly averages of the various climate variables used in the 
                   calculation of the drought indices. The monthly averages can depict seasonality in temperature, precipitation, and
                   potential evapotranspiration (PET) that can aid in the interpretation of different drought index timescales."),
                 br(),
                 plotlyOutput('climoPlotly', width = "auto", height = "400px"),
                 br(),
                 p("Click and drag a box on any part of a time series to zoom in on a specific period. Double click to restore the plot to the full time period."),
                 br(),
                 plotlyOutput('moClimPlotly', width = "auto",height = "800px")),
        tabPanel("About", 
                 tags$div(
                  HTML("<html>
                        <head>
                        <meta content='text/html; charset=ISO-8859-1'
                        http-equiv='content-type'>
                        <title>SDI Viz Tool Info</title>
                        </head>
                        <body>
                        <h2 style='font-family: Helvetica,Arial,sans-serif;'>About
                        the Standardized Drought Index Visualization Tool</h2>
                        <span style='font-family: Helvetica,Arial,sans-serif;'>The
                        Standardized Drought Index Visualization Tool (SDI Viz Tool) was
                        developed to be able to quickly generate and customize multiscale
                        Standardized Precipitation Index (SPI) and Standardized Precipitation
                        Evapotranspiration Index (SPEI) plots. These plots portray all SPI and
                        SPEI timescales, allowing for the visualization of both short and
                        long-term droughts all at once and through time. More information on
                        how to interpret the plots can be found <a target='_blank'
                        href='https://cals.arizona.edu/climate/misc/spi/spicontour.png'>here</a>.
                        More information on the SPI and SPEI can be found at the <a
                        target='_blank' href='https://wrcc.dri.edu/wwdt/about.php'>Westwide
                        Drought Tracker</a>.  <br>
                        <br>
                        The data used in the creation of the plots is the <a target='_blank'
                        href='https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php'>NOAA
                        U.S. Climate Divisional Dataset (nClimDiv)</a>. This dataset is
                        updated through the end of the previous month, usually by the 10th
                        of the current month. Maps of the climate divisions and special
                        regions can be found <a target='_blank'
                        href='https://www.ncdc.noaa.gov/monitoring-references/'>here</a>.
                        &nbsp; <br>
                        </span><br style='font-family: Helvetica,Arial,sans-serif;'>
                        <span style='font-family: Helvetica,Arial,sans-serif;'>The
                        SPI and SPEI values were calculated on the full period of record using the <a
                        href='https://cran.r-project.org/web/packages/SPEI/index.html'
                        target='_blank'>R SPEI package</a>. SDI Viz Tool code
                        can be found at </span><a
                        style='font-family: Helvetica,Arial,sans-serif;'
                        href='https://github.com/mcrimmins/SDIVizTool'>https://github.com/mcrimmins/SDIVizTool</a>.
                        <br>
                        <br>
                        <div style='text-align: center;'><em
                        style='font-family: &quot;Helvetica Neue&quot;,Helvetica,Arial,sans-serif; font-size: 14px; letter-spacing: normal; orphans: 2; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; background-color: rgb(255, 255, 255); font-weight: bold; color: black;'><a
                        href='http://cals.arizona.edu/climate/'
                        style='background-color: transparent; text-decoration: none;'>Climate
                        Science Applications Program - University of Arizona Cooperative
                        Extension</a></em><br>
                        <br>
                        <img style='width: 400px; height: 71px;' alt='logo'
                        src='UA_CSAP_CLIMAS_logos_horiz.png'><br>
                        <br>
                        <span
                        style='color: rgb(51, 51, 51); font-family: &quot;Helvetica Neue&quot;,Helvetica,Arial,sans-serif; font-size: 12px; font-style: normal; font-weight: 500; letter-spacing: normal; orphans: 2; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; background-color: rgb(255, 255, 255); display: inline ! important; float: none;'>©
                        2020 The Arizona Board of Regents. All contents copyrighted. All rights
                        reserved.</span><br
                        style='font-family: Helvetica,Arial,sans-serif;'>
                        </div>
                        <span style='font-family: Helvetica,Arial,sans-serif;'></span><br>
                        <br>
                        <br>
                        </body>
                        </html>"
                   )
                 ))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # ---- Get nClimDiv data ----
  # get county, div and state data ----
  dataSets<-c("climdiv-pcpncy-v",
              "climdiv-pcpndv-v",
              "climdiv-pcpnst-v",
              "climdiv-tmincy-v",
              "climdiv-tmindv-v",
              "climdiv-tminst-v",
              "climdiv-tmaxcy-v",
              "climdiv-tmaxdv-v",
              "climdiv-tmaxst-v")
  # -----
  # container for all data
  datalist = list()
  
  # get directory listing and find most recent prcp file
  url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/'
      # see if FTP is working
      tryCatch(getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose=TRUE), error=function(e) {
        err <<- conditionMessage(e)
      })
      # 
      if(exists("err")==TRUE){
        showModal(modalDialog(title="DOWNLOAD ERROR","NOAA ftp data server not responding - please try again later (notify Mike Crimmins at crimmins@email.arizona.edu if problem persists)", footer=NULL))
      }
  
  # proceed
  filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose=TRUE)
    filelist<-unlist(strsplit(filenames,"\n"))
  
    showModal(modalDialog(title="Please wait","Downloading data...this can take several minutes.", footer=NULL))
        # loop through dataset    
        for(i in 1:length(dataSets)){ 
          # download files and format into list
          tempName<-filelist[which((grepl(dataSets[i], filelist)) == TRUE)]
          url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",tempName)
          tempData<-read.table(url, colClasses = c("character","numeric",
                                                   "numeric","numeric","numeric","numeric","numeric",
                                                   "numeric","numeric","numeric","numeric","numeric",
                                                   "numeric"))
          colnames(tempData)<-c("code",1:12)
          tempData$var<-dataSets[i]
          # add to container
          datalist[[i]] <- tempData
          print(paste0("Downloading ",dataSets[i]))
        }
        # combine into dataframe  
        allData = do.call(rbind, datalist)
        rm(datalist)
        # ----
        # update max.date
        # maxYr<-as.numeric(substr(allData[nrow(allData),1],nchar(allData[nrow(allData),1])-3,nchar(allData[nrow(allData),1])))
        # if(length(which(allData[nrow(allData),]==-99.9))==0){
        #   mm<-12
        #   }else{
        #   mm<-which(allData[nrow(allData),]==-99.9)-2
        # }
        # latest.date<-as.Date(paste0(maxYr,"-",mm,"-01"))
    
    # end of wait message    
    removeModal()
 
# ---- DROP IN PLOTTING CODE
        
  observeEvent(input$updateAll,ignoreNULL=FALSE,{        
    
    showModal(modalDialog("Updating and generating plot", footer=NULL))    
    
        # ---- subset and wrangle data ----  
        # county, division or state? DEAL WITH SPECIAL REGIONS!!
        typePET<-input$petOpt # thornW or harg
        # selections
        region<-input$selectOpt # cy,st, dv, rg
        state <-input$sel_st
        cdiv  <- input$sel_dv
        county<- input$sel_cy
        specReg<-input$sel_rg
        # rg is not a string on climdiv filenames...create new var with ifelse for rg 
        regChr<-region
        regChr<-ifelse(regChr=="rg", "st", regChr)
        # region subset
        tempData<-allData[which(grepl(regChr, allData$var)==TRUE),]
        # parse code col
        if(region=="cy"){
          # get codes
          geoCode<-strsplit(county,"-")
          stCode<- stateCodes[which(stateCodes$name==((geoCode[[1]][1]))),1]
            cyFIPS<-as.character(county.fips[which(county.fips$polyname==paste0(tolower(geoCode[[1]][1]),",",tolower(geoCode[[1]][2]))),1])
            cyFIPS<-as.numeric(ifelse(nchar(cyFIPS)==4, substr(cyFIPS,2,4), substr(cyFIPS,3,5)))
          # parse into columns
          tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
          tempData$div<-(as.numeric(substr(tempData$code, 3,5)))
          tempData$element<-(as.numeric(substr(tempData$code, 6,7)))
          tempData$year<-(as.numeric(substr(tempData$code, 8,11)))
          tempData<-subset(tempData, state==stCode & div==cyFIPS)
          # centroid
          subArea<-subset(countiesPoly,NAME_2==geoCode[[1]][2] & NAME_1==geoCode[[1]][1])
          centroid<-colMeans(coordinates(subArea))
          # build name string
          titleName<-paste0(geoCode[[1]][2]," County,",geoCode[[1]][1])
        }else if (region=="st"){
          # get codes
          stCode<- stateCodes[which(stateCodes$name==state),1]
          # parse into cols
          tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
          tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
          tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
          tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
          tempData<-subset(tempData, state==stCode & div==0)
          # centroid
          subArea<-subset(statesPoly, NAME_1==(state))
          centroid<-colMeans(coordinates(subArea))
          # build name string
          titleName<-paste0((state))
        }else if (region=="dv") {
          # get codes
          geoCode1<-strsplit(cdiv,"-")
          geoCode2<-strsplit(geoCode1[[1]][2],",")
            stCode<- stateCodes[which(stateCodes$name==((geoCode1[[1]][1]))),1]
            cdiv<- as.numeric(geoCode2[[1]][1])
          # parse into cols
          tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
          tempData$div<-(as.numeric(substr(tempData$code, 3,4)))
          tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
          tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
          tempData<-subset(tempData, state==stCode & div==cdiv)
          # centroid
          subArea<-subset(cdivPoly, STATE==geoCode1[[1]][1] & CD_NEW==cdiv)
          centroid<-colMeans(coordinates(subArea))
          # build name string
          titleName<-paste0(geoCode1[[1]][1]," Climate Division ", cdiv)
        }else{
          ### REGION
          # get codes
          stCode<- regionCodes$code[which(regionCodes$name==specReg)]
          # parse into cols
          tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
          tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
          tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
          tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
          tempData<-subset(tempData, state==stCode & div==0)
          # centroid
          subArea<-subset(combinedRegions, region==(specReg))
          centroid<-colMeans(coordinates(subArea))
          # build name string
          titleName<-CapStr(gsub("-", " ", specReg))
          ### REGION FIX
        } 
        
        # melt data
        tempDataMelt<-melt(tempData, id.vars=c(14,18), measure.vars = 2:13)
        #tempDataMelt$date <- as.yearmon(paste(tempDataMelt$year, as.numeric(tempDataMelt$variable), sep = "-"))
        tempDataMelt$date <- as.Date(paste0(tempDataMelt$year,"-",as.numeric(tempDataMelt$variable),"-01"), format="%Y-%m-%d")
        tempDataMelt<-spread(tempDataMelt, var, value)
        # sort, -999 to NA
        tempDataMelt[tempDataMelt == -9.99] <- NA
        tempDataMelt[tempDataMelt == -99.9] <- NA
        # trim to 2018
        #allDataSubset<-allDataSubset[-(which(allDataSubset$year==2019)),]
        # standard colnames
        colnames(tempDataMelt)[4:6]<-c("precip","tmax","tmin")
        # calc tmean
        tempDataMelt$tmean<-(tempDataMelt$tmax+tempDataMelt$tmin)/2
        # ----
        
        # inset map ---- fix MI boundary ----
        insetmap<-ggplot() +
          geom_polygon(data = states4map, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey",size=0.15)  + # get the state border back on top
          geom_polygon(data = subArea, aes(x = long, y = lat, group = group), fill="orange", color="red", size=0.15)  + # get the state border back on top
          coord_fixed(xlim=c(-125, -68), ylim = c(25,50), ratio = 1)+
          #coord_fixed(xlim=c(out$meta$ll[1]-3.5, out$meta$ll[1]+3.5), ylim=c(out$meta$ll[2]-3.5, out$meta$ll[2]+3.5), ratio = 1) +
          #geom_point(data = point, aes(x = V1, y = V2), size=1, color='red')+
          theme_bw(base_size=5)+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())+
          theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
          theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank())
        
        # calculate indices ----
        ## Loop thru full SPI set
        dfSPI<-tempDataMelt[,1:3]
        for(i in 1:60){
          tempSPI <- spi(tempDataMelt$precip,i, na.rm = TRUE)
          dfSPI[[paste('SPI-',i,sep="")]] <-tempSPI$fitted
        }
        # remove rows with NAs
        dfSPI<-na.omit(dfSPI)
        #indexName="Standardized Precipitation Index"
        #indexNameShort="SPI"
        
        # # SPEI switch?
        if (typePET=="thornW"){
          PET <- thornthwaite(fahrenheit.to.celsius(tempDataMelt$tmean,round=2), as.numeric(centroid[2]), na.rm = TRUE) 
        }else{
          PET <- hargreaves(fahrenheit.to.celsius(tempDataMelt$tmin,round=2),fahrenheit.to.celsius(tempDataMelt$tmax,round=2),Ra=NA, as.numeric(centroid[2]), na.rm = TRUE) 
        }
        dfSPEI<-tempDataMelt[,1:3]
        for(i in 1:60){
          tempSPI <- spei(inches_to_metric(tempDataMelt$precip,unit="mm",round=2)-PET,i, na.rm = TRUE)
          dfSPEI[[paste('SPEI-',i,sep="")]] <-tempSPI$fitted
        }
        # remove rows with NAs
        dfSPEI<-na.omit(dfSPEI)
        #indexName="Standardized Precipitation-Evapotranspiration Index"
        #indexNameShort="SPEI"
        
        # monthly anomalies - https://www.r-bloggers.com/visualize-monthly-precipitation-anomalies/
        tempDataMelt$PET<-PET/25.4
        tempDataMelt$P_PET<-tempDataMelt$precip-tempDataMelt$PET
        moAvg <- tempDataMelt %>%
          group_by(variable) %>%
          summarise(moAvgPrecip = mean(precip, na.rm=TRUE),
                    moAvgTemp   = mean(tmean, na.rm=TRUE),
                    moAvgPET    = mean(PET, na.rm=TRUE),
                    moAvgP_PET  = mean(P_PET, na.rm=TRUE))
        moAvg[,2:5] <-round(moAvg[,2:5],2)
        
        tempDataMelt <- left_join(tempDataMelt, moAvg, by = "variable")
        tempDataMelt$precipAnom <- tempDataMelt$precip-tempDataMelt$moAvgPrecip
        tempDataMelt$tempAnom <- tempDataMelt$tmean-tempDataMelt$moAvgTemp
        tempDataMelt$PETAnom <- tempDataMelt$PET-tempDataMelt$moAvgPET
        tempDataMelt$P_PETAnom <- tempDataMelt$P_PET-tempDataMelt$moAvgP_PET
        # anom sign
        tempDataMelt$pAnomSign<-ifelse(tempDataMelt$precipAnom > 0, "pos", "neg")
        tempDataMelt$petAnomSign<-ifelse(tempDataMelt$P_PETAnom > 0, "pos", "neg")
        tempDataMelt$TAnomSign<-ifelse(tempDataMelt$tempAnom > 0, "pos", "neg")
        # round values
        tempDataMelt[,8:17] <-round(tempDataMelt[,8:17],2)
        
        # plot variables ----
        # date range
        dateRange<-input$dateRangeMY
        date1<-dateRange[1]# by month
        date2<-dateRange[2] # by month
        maxScale<-input$maxTimescale+1# max 60
        
        # SPI contour plot ----
        dfSPI<-melt(dfSPI, id.vars=c(1:3), measure.vars = 4:63)
        dfSPI$value<-as.numeric(dfSPI$value)
        colnames(dfSPI)[2]<-"month"
        # current heat map
        currDfSPI<-dfSPI[which(dfSPI$date==date2),]
        # plot  
        pCurr<- ggplot(currDfSPI, aes((date),as.numeric(variable) , fill = value))+
          geom_tile(width=1)+
          scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                               na.value = "grey50", guide = FALSE, limits=c(-3, 3), oob=squish)+
          theme_bw()+
          scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
          scale_x_date(labels = date_format("%b%Y"), expand=c(0,0))+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.x=element_blank())+
          labs(title=" ")+
          theme(plot.margin = unit(c(5, 5, 0, 0), "pt"))
        
        # main plot  
        p1<-  ggplot(dfSPI, aes((date),as.numeric(variable) , fill = value))+
          geom_tile(width=31)+
          #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
          #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
          scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                               na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
          scale_x_date(labels = date_format("%Y-%m"), breaks='2 years', expand=c(0,0),
                       limits = c(as.Date(date1),as.Date(date2)))+
          scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
          theme_bw()+
          theme(legend.position="left")+
          theme(plot.title = element_text(face="bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10))+
          ylab("Timescale (mos)")+
          labs(title=paste0(titleName," Standardized Precipitation Index (", format(as.Date(date1), "%b%Y"),
                            " - ",format(as.Date(date2), "%b%Y"),")"))+
          theme(plot.margin = unit(c(5, 0, 0, 0), "pt"))
        
        # precip anoms
        p2<- ggplot(tempDataMelt,aes(date, precipAnom, fill = pAnomSign)) + 
          geom_bar(stat = "identity", show.legend = FALSE) + 
          #scale_y_continuous(breaks = seq(-100, 100, 20)) +
          scale_fill_manual(values = c("orange4", "darkgreen"))+
          scale_x_date(labels = date_format("%Y"), breaks='2 years', expand=c(0,0),
                       limits = c(as.Date(date1),as.Date(date2)))+
          ylab("Precip Anom (in.)")+
          xlab("Month-Year")+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
          theme(panel.grid.minor = element_blank())
        
        # # trying to get alignments
        #    mainP<-plot_grid(p1, p2, ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
        #    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
        #    plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
        # # another solution
        #    plot_grid(p1,pCurr,p2,NULL, ncol = 2, nrow = 2, align = 'v',axis = 'b', rel_heights = c(10,10,1,1),
        #              rel_widths = c(20,1,20,1))
        
        # plotting grid using align
        mainCurr <- align_plots(p1, pCurr, align = 'h', axis = 'l')
        mainPrec <- align_plots(p1, p2, align = 'v', axis = 'b')
        
        mainP<-plot_grid(mainCurr[[1]], mainPrec[[2]], ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
        sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
        #plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
        spiPlot<-plot_grid(mainP, sideP, nrow = 1, rel_widths = c(20,1))
        
        # add inset map
        spiPlot<-ggdraw(spiPlot)+draw_plot(insetmap, -0.315, 0.40, scale=0.14)
        
        # add margin
        spiPlot = spiPlot + theme(plot.margin = unit(c(0.25, 0.25, 0.7, 0.25), "in")) 
        # add caption
        captionString <- c( "Data from NOAA-NCEI",
                            "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",
                            paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                            "The University of Arizona",
                            "https://cals.arizona.edu/climate/")
        spiPlot<-ggdraw(spiPlot) + draw_text(captionString, x =0.125, 
                                             y = c(0.0625,0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
        
        # write high res to file ----
        png("spiPlot.png", width = 11, height = 8.5, units = "in", res = 300L)
        #grid.newpage()
        print(spiPlot, newpage = FALSE)
        dev.off()
        
        # add logos
        # Call back the plot
        plot <- image_read("spiPlot.png")
        # And bring in a logo
        #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
        logo_raw <- image_read("UA_CSAP_CLIMAS_logos_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
        # Stack them on top of each other
        #final_plot <- image_append((c(plot, logo)), stack = TRUE)
        #final_plot <- image_mosaic((c(plot, logo)))
        final_plot <- image_composite(plot, logo, offset = "+2235+2365")
        # And overwrite the plot without a logo
        image_write(final_plot, "spiPlot.png")  
        # ----   
        # send image file
        output$spiImage <- renderImage({
          # When input$n is 3, filename is ./images/image3.jpeg
          filename <- "spiPlot.png"
          # Return a list containing the filename and alt text
          list(src = filename,
               alt = "SPIPlot")
        }, deleteFile = FALSE)
        
        # PLOTLY SPI HEATMAP ----
        output$SPIPlotly <- renderPlotly({
          plot_ly(dfSPI, x = ~date, y = ~variable, z = ~value, colors=colorRamp(c("orange3","orange","yellow","white","green","green2","darkgreen")),
                  type = "heatmap", zmin=-3, zmax=3) %>%
          layout(title = paste0(titleName," Standardized Precipitation Index (", format(as.Date(date1), "%b%Y"),
                                " - ",format(as.Date(date2), "%b%Y"),")"),
                 xaxis=list(title="Month-Year",
                            range = c(as.Date(date1),as.Date(date2))),
                 yaxis=list(title="Timescale (mos)",
                            range = c(0,maxScale))
          )
        })
        # ----         
        
        
        # SPEI contour plot ----
        dfSPEI<-melt(dfSPEI, id.vars=c(1:3), measure.vars = 4:63)
        dfSPEI$value<-as.numeric(dfSPEI$value)
        colnames(dfSPEI)[2]<-"month"
        # current heat map
        currDfSPEI<-dfSPEI[which(dfSPEI$date==date2),]
        # plot  
        pCurr<- ggplot(currDfSPEI, aes((date),as.numeric(variable) , fill = value))+
          geom_tile(width=1)+
          scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                               na.value = "grey50", guide = FALSE, limits=c(-3, 3), oob=squish)+
          theme_bw()+
          scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
          scale_x_date(labels = date_format("%b%Y"), expand=c(0,0))+
          theme(axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                axis.title.x=element_blank())+
          labs(title=" ")+
          theme(plot.margin = unit(c(5, 5, 0, 0), "pt"))
        
        # main plot  
        p1<-  ggplot(dfSPEI, aes((date),as.numeric(variable) , fill = value))+
          geom_tile(width=31)+
          #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
          #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
          scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                               na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
          scale_x_date(labels = date_format("%Y-%m"), breaks='2 years', expand=c(0,0),
                       limits = c(as.Date(date1),as.Date(date2)))+
          scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
          theme_bw()+
          theme(legend.position="left")+
          theme(plot.title = element_text(face="bold"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          guides(fill = guide_colourbar(barwidth = 1, barheight = 10))+
          ylab("Timescale (mos)")+
          labs(title=paste0(titleName," Standardized Precipitation-Evapotranspiration Index (", format(as.Date(date1), "%b%Y"),
                            " - ",format(as.Date(date2), "%b%Y"),")"))+
          theme(plot.margin = unit(c(5, 0, 0, 0), "pt"))
        
        # precip anoms
        p2<- ggplot(tempDataMelt,aes(date, P_PETAnom, fill = petAnomSign)) + 
          geom_bar(stat = "identity", show.legend = FALSE) + 
          #scale_y_continuous(breaks = seq(-100, 100, 20)) +
          scale_fill_manual(values = c("orange4", "darkgreen"))+
          scale_x_date(labels = date_format("%Y"), breaks='2 years', expand=c(0,0),
                       limits = c(as.Date(date1),as.Date(date2)))+
          ylab("P-PET Anom (in.)")+
          xlab("Month-Year")+
          theme_bw()+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
          theme(panel.grid.minor = element_blank())
        
        # # trying to get alignments
        #    mainP<-plot_grid(p1, p2, ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
        #    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
        #    plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
        # # another solution
        #    plot_grid(p1,pCurr,p2,NULL, ncol = 2, nrow = 2, align = 'v',axis = 'b', rel_heights = c(10,10,1,1),
        #              rel_widths = c(20,1,20,1))
        
        # plotting grid using align
        mainCurr <- align_plots(p1, pCurr, align = 'h', axis = 'l')
        mainPrec <- align_plots(p1, p2, align = 'v', axis = 'b')
        
        mainP<-plot_grid(mainCurr[[1]], mainPrec[[2]], ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
        sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
        #plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
        spiPlot<-plot_grid(mainP, sideP, nrow = 1, rel_widths = c(20,1))
        
        # add inset map
        spiPlot<-ggdraw(spiPlot)+draw_plot(insetmap, -0.315, 0.40, scale=0.14)
        
        # add margin
        spiPlot = spiPlot + theme(plot.margin = unit(c(0.25, 0.25, 0.7, 0.25), "in")) 
        # add caption
        captionString <- c( "Data from NOAA-NCEI",
                            "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",
                            paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                            "The University of Arizona",
                            "https://cals.arizona.edu/climate/")
        spiPlot<-ggdraw(spiPlot) + draw_text(captionString, x =0.125, 
                                             y = c(0.0625,0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
        
        
        # write high res to file ----
        png("speiPlot.png", width = 11, height = 8.5, units = "in", res = 300L)
        #grid.newpage()
        print(spiPlot, newpage = FALSE)
        dev.off()
        
        # add logos
        # Call back the plot
        plot <- image_read("speiPlot.png")
        # And bring in a logo
        #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
        logo_raw <- image_read("UA_CSAP_CLIMAS_logos_horiz.png") 
        logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
        # Stack them on top of each other
        #final_plot <- image_append((c(plot, logo)), stack = TRUE)
        #final_plot <- image_mosaic((c(plot, logo)))
        final_plot <- image_composite(plot, logo, offset = "+2235+2365")
        # And overwrite the plot without a logo
        image_write(final_plot, "speiPlot.png")  
        # ----   
        
        # send image file
        output$speiImage <- renderImage({
          # When input$n is 3, filename is ./images/image3.jpeg
          filename <- "speiPlot.png"
          # Return a list containing the filename and alt text
          list(src = filename,
               alt = "SPEIPlot")
        }, deleteFile = FALSE)
        
        # PLOTLY SPI HEATMAP ----
        output$SPEIPlotly <- renderPlotly({plot_ly(dfSPEI, x = ~date, y = ~variable, z = ~value, colors=colorRamp(c("orange3","orange","yellow","white","green","green2","darkgreen")),
                                                   type = "heatmap", zmin=-3, zmax=3) %>%
          layout(title = paste0(titleName," Standardized Precipitation-Evapotranspiration Index (", format(as.Date(date1), "%b%Y"),
                                " - ",format(as.Date(date2), "%b%Y"),")"),
                 xaxis=list(title="Month-Year",
                            range = c(as.Date(date1),as.Date(date2))),
                 yaxis=list(title="Timescale (mos)",
                            range = c(0,maxScale))
          )
        })
        # ----         
        
        # PLOTLY diff plot ----
        tempPlotlyDF<-as.data.frame(cbind(dfSPI$variable,dfSPI$value-dfSPEI$value))
        colnames(tempPlotlyDF)<-c("variable","value")
        tempPlotlyDF$date<-dfSPI$date
        output$diffPlotly <- renderPlotly({plot_ly(tempPlotlyDF, x = ~date, y = ~variable, z = ~value, colors='PuOr', type = "heatmap", zmin=-2, zmax=2) %>%
          layout(title = paste0(titleName," SPI-SPEI (", format(as.Date(date1), "%b%Y"),
                                " - ",format(as.Date(date2), "%b%Y"),")"),
                 xaxis=list(title="Month-Year",
                            range = c(as.Date(date1),as.Date(date2))),
                 yaxis=list(title="Timescale (mos)",
                            range = c(0,maxScale))
          ) 
        })
        # ---- 
        
        # interactive plots of temp, precip, PET, Anoms ----
        # temp Plotly
        tempPlotlyVars<-tempDataMelt[,c(3,5,6,7)]
        colnames(tempPlotlyVars)<-c("date","T-max(F)","T-mean(F)","T-min(F)")
        tempPlots<-tempPlotlyVars %>%
          tidyr::gather(variable,value,-date) %>%
          transform(id = as.integer(factor(variable))) %>%
          plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("dodgerblue4","dimgray","firebrick"),
                  yaxis = ~paste0("y", id)) %>%
          add_lines()
        # temp Anom Plotly
        tempPlotlyVars<-tempDataMelt[,c(3,15)]
        colnames(tempPlotlyVars)<-c("date","T-mean Anom(F)")
        tempAnomPlots<-tempPlotlyVars %>%
          tidyr::gather(variable,value,-date) %>%
          transform(id = as.integer(factor(variable))) %>%
          plot_ly(x = ~date, y = ~value, color = ~variable, colors = "dimgray",
                  yaxis = ~paste0("y", id)) %>%
          add_lines()
        # precip Plotly
        tempPlotlyVars<-tempDataMelt[,c(3,4,14)]
        colnames(tempPlotlyVars)<-c("date","Precip(in)","PrecipAnom(in)")
        precipPlots<-tempPlotlyVars %>%
          tidyr::gather(variable,value,-date) %>%
          transform(id = as.integer(factor(variable))) %>%
          plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("forestgreen","darkslateblue"),
                  yaxis = ~paste0("y", id)) %>%
          add_lines()
        # Precip Anom Plotly
        # tempPlotlyVars<-tempDataMelt[,c(3,14)]
        # precipAnomPlots<-tempPlotlyVars %>%
        #   tidyr::gather(variable,value,-date) %>%
        #   transform(id = as.integer(factor(variable))) %>%
        #   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgreen",
        #           yaxis = ~paste0("y", id)) %>%
        #   add_lines()
        # PET Plotly
        tempPlotlyVars<-tempDataMelt[,c(3,8)]
        colnames(tempPlotlyVars)<-c("date","PET(in)")
        PETPlots<-tempPlotlyVars %>%
          tidyr::gather(variable,value,-date) %>%
          transform(id = as.integer(factor(variable))) %>%
          plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgoldenrod",
                  yaxis = ~paste0("y", id)) %>%
          add_lines()
        # PET_P Plotly
        tempPlotlyVars<-tempDataMelt[,c(3,9,17)]
        colnames(tempPlotlyVars)<-c("date","Precip-PET(in)","Precip-PETAnom(in)")
        PET_PPlots<-tempPlotlyVars %>%
          tidyr::gather(variable,value,-date) %>%
          transform(id = as.integer(factor(variable))) %>%
          plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("darkorange","darkorchid4"),
                  yaxis = ~paste0("y", id)) %>%
          add_lines()
        # Precip Anom Plotly
        # tempPlotlyVars<-tempDataMelt[,c(3,17)]
        # PET_PAnomPlots<-tempPlotlyVars %>%
        #   tidyr::gather(variable,value,-date) %>%
        #   transform(id = as.integer(factor(variable))) %>%
        #   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgreen",
        #           yaxis = ~paste0("y", id)) %>%
        #   add_lines()
        # combine in subplots
        pSubPlot<-subplot(tempPlots, tempAnomPlots, precipPlots,
                          PETPlots, PET_PPlots, nrows = 6, shareX = TRUE)
        # render Plotly
        output$moClimPlotly <- renderPlotly({pSubPlot<-layout(pSubPlot, title=paste0(titleName," Monthly Climate Data"))
          })
        # ----      
        
        # climograph https://plot.ly/r/multiple-axes/ ----
        ay <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "Temp(F)"
        )
        pClimo <- plot_ly() %>%
          add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgPrecip, name = "Precip(in)") %>%
          add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgPET, name = "PET(in)") %>%
          add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgP_PET, name = "P-PET(in)") %>%
          add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgTemp, name = "Temp(F)", yaxis = "y2") %>%
          layout(
            title = paste0(titleName," Monthly Average Climate"), yaxis2 = ay,
            xaxis = list(title="month",
                         range=c(1,12)),
            yaxis = list(title="inches")
          )
        
        output$climoPlotly <- renderPlotly({pClimo
          })
        removeModal()
        
    } )
        
# ---- END PLOTTING CODE        
        
}

# Run the application 
shinyApp(ui = ui, server = server)

