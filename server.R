#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : March 2022 (to include SP)
#' 
#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_TZ -testSP")


library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(shinyalert)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
library(formattable)
library(shinybusy)
library(DT)
library(shinyjs)

source("SP_functions.R")
### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
  

      # 
  dataModal <- function(failed = FALSE) {
    list(
    modalDialog(
      #span("AKILIMO Paper Based Annex dashboard"),
      size = c("m"),
      HTML('<img src="pics/akilimo4.jpg" />'),
      
       br(),
      # br(),
      selectInput("use", "Please select the use case",
                  choices = c("Fertilizer Recommendation", "Scheduled Planting"),
                  selected = ""),
 
      
      easyClose = FALSE,
      fade = TRUE,
     
      
      # if (failed)
      #   div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        #modalButton("Cancel"),
        useShinyjs(),  # Set up shinyjs
        actionButton("ok", "DONE!")
      )
    ),
    tags$style("
#shiny-modal .modal-dialog {
  position: absolute;
  top: 150px;
  right: 10px;
  left: 200px;
  bottom: 50;
  left: 0;
  z-index: 10040;
 
}")
    )
  }
  

    showModal(
        dataModal()
    )
    
    observeEvent(input$ok, {
      
      if(input$use == "Fertilizer Recommendation"){
    shinyalert("Fertilizer Recommendation", "This tool contains tables and maps with advice on application rates of urea,
                       NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer 
                       depends on soil conditions and the time of planting.

                                 This window will automatically close once country data is loaded.


               ",
               
               type = "info", timer = 7000, size = 'm',
               closeOnClickOutside = FALSE,
               closeOnEsc = FALSE,
               animation = FALSE,
               html = FALSE,
        
               showConfirmButton = FALSE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#AEDEF4")
      }
  })
    
    # showModal(dataModal())
    
    observeEvent(input$ok, {
      
      if(input$use == "Scheduled Planting"){
        shinyalert("Loading planting and harvest schedules...",  "
                                            This tool provides expected cassava yields for different planting and harvest schedules. 
                                            These are yields predicted based on the common rainfall pattern between the month of planting 
                                            and harvest, and assume good agronomic practices are applied. For every State and planting 
                                            month, a table is generated, presenting the expected root yield for different LGAs (in the rows) 
                                            and harvest from 8 up to 15 months after planting (in the columns).

                                            This window will automatically close when the data is loaded.


               ",
                   type = "info", timer = 7000, size = 'm',
                   closeOnClickOutside = FALSE,
                   closeOnEsc = FALSE,
                   animation = FALSE,
                   html = FALSE,
                   
                   showConfirmButton = FALSE,
                   showCancelButton = FALSE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#AEDEF4")
      }
    })
  

  #............................................................................. 
  #spinner before maps are displayed
  #.............................................................................
  observeEvent(input$btn_go, {
  
    shinybusy::show_modal_spinner(
   
      spin = "cube-grid",
      #spin = "fading-circle",
      #spin = "fading-circle",
     
      color = 	"#228B22",
      #00FF00
      text = "Please wait while the map is being generated...",
    )

    Sys.sleep(6)
    remove_modal_spinner()
  })
  


  output$region <- renderUI({
    
    pickerInput("region", "Select region", choices = c("Geita", "Kagera","Kigoma","Lindi", "Mara", "Mtwara", "Mwanza","Pwani", "Shinyanga",  
                                                       "Simiyu", "Tanga","Zanzibar South and Central"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
 
  observeEvent(input$region, { 
    if(!is.null(input$region))  {
  output$plntmth <- renderUI({
        
      pickerInput("plntmth", "Select planting month",
                  choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                              "October", "November", "December"),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1))
  })
    }
  })  

  

  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth)) {
      output$costs <- renderUI({
        
        pickerInput("costs", "Would you like to specify your prices for cassava and fertilizers?",
                    choices = c("Yes", "No"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
  
    }
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth))  {
  output$selection <- renderUI({
    
    pickerInput("selection2", "Select variable to view",
                choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
    }
  })


  observeEvent(input$region, {
    if(!is.null(input$region))  {
      
      output$unit_loc <- renderUI({
        
        selectInput("unit_loc", "Select unit of land",
                    choices = c("acre", "hectare"))
      })
    }
  }) 
  

  observeEvent(input$regionSP, {
    if(!is.null(input$regionSP))  {

      output$unit_locSP <- renderUI({

        selectInput("unit_locSP", "Select unit of land",
                    choices = c("acre", "hectare"))

      })
    }
  })
  

  
  observeEvent(input$unit_loc, {
    if(!is.null(input$unit_loc))  {
      output$FCY_ha <- renderUI({

        selectInput("FCY_ha", "Select Your Current Yield (Tonnes)",
                                  choices = c("0-7.5 t/hectare", "7.5-15 t/hectare", "15-22.5 t/hectare", "22.5-30 t/hectare", ">30 t/hectare",  ""),
                    selected = "")            
      })
    

      output$FCY_acre <- renderUI({

        selectInput("FCY_acre", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/acre", "3-6 t/acre", "6-9 t/acre", "9-12 t/acre", ">12 t/acre", ""),
                    selected = "")
      })
    }
      
    })  
  

  observeEvent(input$costs, {
    if(input$costs == "Yes" ) {
   
  output$CassavaPrice <- renderUI({
    
    textInput("CassavaPrice", "Price of cassava per ton")
  })
    }
  })

  
  observeEvent(input$costs, {
    if(input$costs == "Yes")  {
      
      output$NPK171717Price <- renderUI({
        
        textInput("NPK171717Price", "Cost of NPK:17:17:17 per 50Kg bag")
      })
      
      
      output$DAPPrice <- renderUI({
        
        textInput("DAPPrice", "Cost of DAP per 50Kg bag")
      })
      
    }
  })
  

    observeEvent(input$costs, {
      if(input$costs == "Yes") {
      output$UreaPrice <- renderUI({
        
        textInput("UreaPrice", "Cost of Urea per 50Kg bag")
      })
    }
    })

  
  observeEvent(input$costs, {
    if(input$costs == "Yes") {  
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }else if(input$costs == "No"){
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }
      
    
  })
  
  observeEvent(input$use, {
    if (input$use == "Scheduled Planting"){
      showTab(inputId = "nav", target = "Expected yield for different planting and harvest schedules", select = TRUE)
      hideTab(inputId = "nav", target = "Use case mapper")
      hideTab(inputId = "nav", target = "View maps side by side")
      hideTab(inputId = "nav", target = "View Table")
    }
  })

  observeEvent(input$use, {
  if(input$use == "Fertilizer Recommendation"){
    hideTab(inputId = "nav", target = "Expected yield for different planting and harvest schedules")
    showTab(inputId = "nav", target = "Use case mapper", select = TRUE)
    showTab(inputId = "nav", target = "View maps side by side")
    showTab(inputId = "nav", target = "View Table")
     }
  })
 
 #.................................................................................................................
  ## Determine platform type and set working directory accordingly
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    if(!is.null(input$ok)){
      removeModal()
    }
  })
  
  
  observeEvent(input$ok, {
    
    if (input$use == "Fertilizer Recommendation"){

  
      #######################################################################################
      ## Read the GIS layers
      #######################################################################################
      
      boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
      
      
      tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")
      
      
      
      ###########################################################################
      ##  TZ fertilizer recom for FCY 1:5
      ###########################################################################
      FR_TZ_FCY1 <- readRDS("FRrecom_lga_level1_TZ_2020.RDS")
      FR_TZ_FCY2 <- readRDS("FRrecom_lga_level2_TZ_2020.RDS")
      FR_TZ_FCY3 <- readRDS("FRrecom_lga_level3_TZ_2020.RDS")
      FR_TZ_FCY4 <- readRDS("FRrecom_lga_level4_TZ_2020.RDS")
      FR_TZ_FCY5 <- readRDS("FRrecom_lga_level5_TZ_2020.RDS")
      
      
      
      ###########################################################################
      ##  adding planting month
      ###########################################################################
      addplm <- function(ds, country){
        ds$respY <- ds$TargetY - ds$CurrentY
        ds$groRev <- ds$NR + ds$TC
        ds$plm <- as.factor(ds$plw)
        levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
        levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
        levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
        levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
        levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
        levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
        levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
        levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
        levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
        levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
        levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
        levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
        
        
        ds$rateUrea <- ds$urea
        ds$rateNPK171717 <- ds$NPK17_17_17
        ds$rateDAP <- ds$DAP
        
        return(ds)
      }
      
      
      FR_TZ_FCY1_plm <- addplm(ds=FR_TZ_FCY1, country = "TZ") ## TZ if user current yield is level 1
      FR_TZ_FCY2_plm <- addplm(ds=FR_TZ_FCY2, country = "TZ") ## TZ if user current yield is level 2
      FR_TZ_FCY3_plm <- addplm(ds=FR_TZ_FCY3, country = "TZ") ## TZ if user current yield is level 3
      FR_TZ_FCY4_plm <- addplm(ds=FR_TZ_FCY4, country = "TZ") ## TZ if user current yield is level 4
      FR_TZ_FCY5_plm <- addplm(ds=FR_TZ_FCY5, country = "TZ") ## TZ if user current yield is level 5
 
    
      removeModal()
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 

    observeEvent(input$btn_go, {
      
      #define reactive values
     # country <- input$country
      FCY_ha <- input$FCY_ha
      
      FCY_acre <- input$FCY_acre
      
      print(FCY_acre)
      
      Selection2 <- input$selection2
      usecase <- input$usecase
      plantMonth <- input$plntmth
      state <- input$state
      #region <- input$region
      
    
      plantMonth <- input$plntmth
     
      unit <- input$unit_loc
      
      UreaPrice <- as.numeric(input$UreaPrice)
      DAPPrice <- as.numeric(input$DAPPrice)
      
      NPK171717Price <- as.numeric(input$NPK171717Price)
      
      CassavaPrice <- as.numeric(input$CassavaPrice)
      
      costs <- input$costs
      
      print(unit)
      
      #specify yield categories
      
      if(unit == 'hectare'){
        yield_level <- ifelse( FCY_ha == "0-7.5 t/hectare", "a low yield level",
                               ifelse( FCY_ha == "7.5-15 t/hectare","a normal yield level",
                                       ifelse( FCY_ha == "15-22.5 t/hectare","a medium yield level", 
                                               ifelse( FCY_ha == "22.5-30 t/hectare","a high yield level",
                                                       ifelse( FCY_ha == ">30 t/hectare","a very high yield level"
                                                       )))))
      }else if(unit == 'acre'){ 
        yield_level <- ifelse( FCY_acre == "0-3 t/acre","a low yield level",
                               ifelse( FCY_acre == "3-6 t/acre","a normal yield level",
                                       ifelse( FCY_acre == "6-9 t/acre","a medium yield level",
                                               ifelse( FCY_acre == "9-12 t/acre","a high yield level",
                                                       ifelse( FCY_acre == ">12 t/acre","a very high yield level")
                                               ))))
      }
      
      print(yield_level)
      
      #lgaGroups <- "Kigoma"
      lgaGroups <- input$region
    
      lgaGroups2 <- input$region
      
      #define the yield category
      if (unit == "hectare"){
        FCY <- FCY_ha
        
        if(FCY == "7.5-15 t/hectare" ){
          ds <- FR_TZ_FCY2_plm
        }else if(FCY == "0-7.5 t/hectare" ){
          ds <- FR_TZ_FCY1_plm
        }else if(FCY == "15-22.5 t/hectare" ){
          ds <- FR_TZ_FCY3_plm
        }else if(FCY == "22.5-30 t/hectare"){
          ds <- FR_TZ_FCY4_plm
        }else if(FCY == ">30 t/hectare" ){
          ds <- FR_TZ_FCY5_plm
        }
      }else if(unit == "acre"){
        FCY <- FCY_acre
        
        if(FCY == "3-6 t/acre" ){
          ds <- FR_TZ_FCY2_plm
        }else if(FCY == "0-3 t/acre" ){
          ds <- FR_TZ_FCY1_plm
        }else if(FCY == "6-9 t/acre" ){
          ds <- FR_TZ_FCY3_plm
        }else if(FCY == "9-12 t/acre" ){
          ds <- FR_TZ_FCY4_plm
        }else if(FCY == ">12 t/acre" ){
          ds <- FR_TZ_FCY5_plm
          
        }
      }
      
      #subset dataset by regions
      Mara <- droplevels(ds[ds$REGION %in% c("Mara"), ])
      Simiyu <- droplevels(ds[ds$REGION %in% c("Simiyu"), ])
      Maralabel <- data.frame(REGION= c("Mara"), lon=c(34.7), lat=c(-1.2))
      Simiyulabel <- data.frame(REGION= c("Simiyu"), lon=c(33.7), lat=c( -3.7))
      
      Kagera <- droplevels(ds[ds$REGION %in% c("Kagera"), ])
      Geita <- droplevels(ds[ds$REGION %in% c("Geita"), ])
      Kigoma <- droplevels(ds[ds$REGION %in% c("Kigoma"), ])
      
      Kageralabel <- data.frame(REGION= c("Kagera"), lon=c(30.25), lat=c(-2.1))
      Geitalabel <- data.frame(REGION= c("Geita"), lon=c( 32.4), lat=c(-4))
      Kigomalabel <- data.frame(REGION= c("Kigoma"), lon=c(31), lat=c(-6.1))
      
      Mwanza <- droplevels(ds[ds$REGION %in% c("Mwanza"), ])
      Shinyanga <- droplevels(ds[ds$REGION %in% c("Shinyanga"), ])
      Mwanzalabel <- data.frame(REGION= c("Mwanza"), lon=c(33.65), lat=c(-2.1))
      Shinyangalabel <- data.frame(REGION= c("Shinyanga"), lon=c(33.2), lat=c(-4.1))
      
      Tanga <- droplevels(ds[ds$REGION %in% c("Tanga"), ])
      Pwani <- droplevels(ds[ds$REGION %in% c("Pwani"), ])
      Tangalabel <- data.frame(REGION= c("Tanga"), lon=c(37.6), lat=c(-4.8))
      Pwanilabel <- data.frame(REGION= c("Pwani"), lon=c(37.9), lat=c(-7.3))
      
      Mtwara <- droplevels(ds[ds$REGION %in% c("Mtwara"), ])
      Lindi <- droplevels(ds[ds$REGION %in% c("Lindi"), ])
      Mtwaralabel <- data.frame(REGION= c("Mtwara"), lon=c(37.5), lat=c(-11.2))
      Lindilabel <- data.frame(REGION= c("Lindi"), lon=c(338.7), lat=c(-8.1))
      
      Zanzibar <- droplevels(ds[ds$REGION %in% c("Zanzibar South and Central", "Zanzibar West", "Zanzibar North"), ])
      Zanzibarlabel <- data.frame(REGION= c("Zanzibar"), lon=c(39.5), lat=c(-5.95))
      
      Maracity <- data.frame(REGION = c("Mara", "Simiyu"),name=c("Musoma","Bariadi"), lat=c(-1.5,-2.8), lon = c(33.8, 33.98))
      
      Kageracity <- data.frame(REGION = c("Kagera", "Geita", "Kigoma"), name=c("Bukoba","Geita","Kigoma"), 
                               lat=c(-1.33, -2.87, -4.88), lon = c(31.82, 32.23,29.63))
      
      Pwaniacity <- data.frame(REGION = c("Pwani", "Tanga"),name=c("Kibaha","Tanga"), 
                               lat=c(-6.77, -5.07), lon = c(38.92, 39.01))
      
      Mwanzacity <- data.frame(REGION = c("Mwanza", "Shinyanga"),name=c("Mwanza", "Shinyanga"), 
                               lat=c(-2.52, -3.66), lon = c(32.9, 33.42))
      
      Mtwaraacity <- data.frame(REGION = c("Mtwara","Lindi"),name=c("Mtwara","Lindi"),
                                lat=c(-10.27, -9.99), lon = c(40.18, 39.71))
      Zanzibarcity <- data.frame(REGION = "Zanzibar",name="Zanzibar", lat=-6.17, lon = 39.2)
      
      
      if(lgaGroups =="Mtwara"){
        LGApoints <- Mtwara
        stateLabel <- Mtwaralabel
        textangle<-0
        cities = Mtwaraacity
        couple <- "Two"
        
      }else if(lgaGroups =="Lindi"){
        LGApoints <- Lindi
        stateLabel <- Lindilabel
        textangle<-0
        cities = Mtwaraacity
        couple <- "Two"
        
      }else if(lgaGroups =="Pwani"){
        LGApoints <- Pwani 
        stateLabel <- Pwanilabel
        textangle<-0 
        cities = Pwaniacity
        couple <- "Two"
        
      }else if(lgaGroups =="Tanga"){
        LGApoints <- Tanga 
        stateLabel <- Tangalabel
        textangle<-0 
        cities = Pwaniacity
        couple <- "Two"
        
      }else if(lgaGroups =="Mwanza"){
        LGApoints <- Mwanza 
        stateLabel <- Mwanzalabel
        textangle<-0 
        cities = Mwanzacity
        couple <- "Two"
        
      }else if(lgaGroups =="Shinyanga"){
        LGApoints <- Shinyanga 
        stateLabel <- Shinyangalabel
        textangle<-0 
        cities = Mwanzacity
        couple <- "Two"
        
      }else if(lgaGroups =="Mara"){
        LGApoints <- Mara 
        stateLabel <- Maralabel 
        textangle<-0 
        cities = Maracity
        couple <- "Two"
        
      }else if(lgaGroups =="Simiyu"){
        LGApoints <- Simiyu 
        stateLabel <- Simiyulabel 
        textangle<-0 
        cities = Maracity
        couple <- "Two"
        
      }else if(lgaGroups =="Kagera"){
        LGApoints <- Kagera 
        stateLabel <- Kageralabel 
        textangle<-0 
        cities = Kageracity
        couple <- "Two"
        
      }else if(lgaGroups =="Geita"){
        LGApoints <- Geita 
        stateLabel <- Geitalabel 
        textangle<-0 
        cities = Kageracity
        couple <- "Two"
        
      }else if(lgaGroups =="Kigoma"){
        LGApoints <- Kigoma 
        stateLabel <- Kigomalabel 
        textangle<-0 
        cities = Kageracity
        couple <- "Two"
        
      }else if(lgaGroups ==c("Zanzibar South and Central")){
        LGApoints <- Mwanza 
        stateLabel <- Mwanzalabel 
        textangle<-0 
        cities = Zanzibarcity
        couple <- "Two"
        
      }else if(lgaGroups ==c("Zanzibar West")){
        LGApoints <- Mwanza 
        stateLabel <- Mwanzalabel 
        textangle<-0 
        cities = Zanzibarcity
        couple <- "Two"
        
      }else if(lgaGroups ==c("Zanzibar North")){
        LGApoints <- Mwanza 
        stateLabel <- Mwanzalabel 
        textangle<-0 
        cities = Zanzibarcity
        couple <- "Two"
      }
      
      
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
      
      if(couple == "Two"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
      }
      
      if(couple == "Three"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
      }
      
      plotData <- droplevels(plotData[plotData$REGION %in% lgaGroups, ])
      
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryTZ, NAME_1 %in% AOI ) 
      
      AOIMap <- subset(tzRegion, NAME_1 %in% AOI )
      AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("REGION","DISTRICT","long","lat"  )
      crop_ngstate <- subset(tzRegion, NAME_1 %in% AOI )
      
      
      ## take REGION average
      LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                          LGAUrea = round(mean(rateUrea), digits=0),
                          LGANPK171717 = round(mean(rateNPK171717), digits=0),
                          LGADAP = round(mean(rateDAP), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK171717 <- dss$LGANPK171717 / 2.47105
      dss$LGADAP <- dss$LGADAP / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      if(unit == 'acre'){
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
      
      if(unit == "hectare"){
        plotData$Urea <- round(plotData$LGAUrea/25)*25
        plotData$NPK17_17_17 <- round(plotData$LGANPK171717/50)*50
        plotData$DAP <- round(plotData$LGADAP/25)*25
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK17_17_17 <- round(plotData$LGANPK171717/20)*20
        plotData$DAP <- round(plotData$LGADAP/10)*10
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      fileNameCsv <- paste("tables", ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP","dY", "LGAdY")]),
                       by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","DISTRICT"))
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
      
      Currency <- "TZS"
      tt_tz <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP", "LGAdY", "month")]))
      tt_tz$LGAdY <- round(tt_tz$LGAdY, digits = 1)
      tt_tz2 <- dplyr::select(tt_tz, c(REGION, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY))
      
      
      colnames(tt_tz2) <- c("Region","DISTRICT", "Urea (kg/hectare)", "NPK 17:17:17 (kg/hectare)", "DAP kg/hectare", "Expected yield response (t))"
      )
      
      print(input$UreaPrice)
      print(input$NPK171717Price)
      print(input$CassavaPrice)
      print(input$DAPPrice)
      print(input$region)
    
      
      #table output based on cost inputs
      
      
      if(costs == "No"){
         output$tabletext_tzs <- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
          
        })
        
        output$mytable <- renderDT({tt_tz2},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_tz2),
                                                  initComplete = DT::JS(
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                    "}"),
                                                  
                                                  buttons = list(
                                                    list(extend = 'excel', 
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
                                                    list(extend = 'pdf',
                                                         filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                         title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
                                                         header = TRUE)
                                                  )
                                                  
                                   )
        )
        
      }else if (costs == "Yes"){
        #colnames(tt) <- c("State","LGA", "Recommended urea rate (kg/ha)", "NPK15_15_15 rate", "Expected yield response", "Planting month")
        tt_dataframe2 <- reactive({
          
          df_tt2 <- data.frame(UreaPrice=as.numeric(input$UreaPrice),NPK171717Price=as.numeric(input$NPK171717Price),CassavaPrice=as.numeric(input$CassavaPrice),DAPPrice=as.numeric(input$DAPPrice),
                               REGION=input$region)
          
          return(df_tt2)
        })
  
        tt_merge_tz <- merge(tt_tz, tt_dataframe2(),by="REGION")
        
        tt_merge_tz$totalSalePrice = tt_merge_tz$LGAdY  * tt_merge_tz$CassavaPrice
        tt_merge_tz$totalCost = (tt_merge_tz$UreaPrice/50 * tt_merge_tz$Urea) + 
          (tt_merge_tz$NPK171717Price/50 * tt_merge_tz$NPK17_17_17)+
          (tt_merge_tz$DAPPrice/50 * tt_merge_tz$DAP)
         
        tt_merge_tz$NetRevenue = tt_merge_tz$totalSalePrice - tt_merge_tz$totalCost
        
        tt_merge_tz2 <- dplyr::select(tt_merge_tz, c(REGION, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
        colnames(tt_merge_tz2) <- c("Region","District", "Urea (kg/hectare)", "NPK 17:17:17 (kg/hectare)", "DAP", "Expected yield increase (t)", 
                                    "Cassava Price", "Total sale (TZS)", "Fertilizer cost (TZS)", "Profit (TZS)")
        
        
        write.csv(tt_merge_tz2, fileNameCsv, row.names = FALSE)
        
        AOIMap3 <- st_as_sf(AOIMap2)
        
        output$tabletext_tzs <- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
          
        })
        
        output$mytable2 <- renderDT({tt_merge_tz2},
                                    rownames = FALSE, 
                                    extensions = c('Buttons','FixedColumns'), 
                                    options = list(dom = 'Bfrtip',
                                                   pageLength = nrow(tt_merge_tz2),
                                                   initComplete = DT::JS(
                                                     "function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                     "}"),
                                                   
                                                   buttons = list(
                                                     list(extend = 'excel', 
                                                          filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                          title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")),
                                                     list(extend = 'pdf',
                                                          filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                          title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
                                                          header = TRUE)
                                                   )
                                                   
                                    )
        )
        
      }
      # --------------------------------------------------------------------------
      #side by side maps
      # --------------------------------------------------------------------------
      AOIMap3 <- st_as_sf(AOIMap2)
      #urea plot
      ################################################
      
      #reactive  title based on unit of land
      tturea <- reactive({
        
        if(unit == "hectare"){
          
          tturea <- paste("Recommended urea rate(kg/hectare)")
        }else {
          
          tturea <- paste("Recommended urea rate(kg/acre)")
        }
      })
      
      
      
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel)
      library(tmap)
      
      #tmap output using reactive values  
      #urea
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           #breaks = c(200, 175, 150, 125,100),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
      ############################################################################   
      #npk plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk <- reactive({
        
        if(unit == "hectare"){
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/hectare)")
        }else {
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate(kg/acre)")
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK17_17_17)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
      levels(AOIMap3$NPK17_17_17) <- kev
      
      #npk plot
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot_tr <- renderTmap({
                       
                       
                       
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK17_17_17",
                           title = ttnpk(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Oranges")+
                         tm_text(text = "NAME_2")
                       
                       sm2
                       
                     }) 
                   })
      
      
      ############################################################################   
      #dap plot
      #############################################################################
      #reactive title based on unit of land  
      
      ttdap <- reactive({
        
        if(unit == "hectare"){
          
          ttdap <- paste("Recommended DAP (kg/hectare)")
        }else {
          
          ttdap <- paste("Recommended DAP (kg/acre)")
        }
      })
      
      
      
      dapsclae <- unique(AOIMap3$DAP)
      kedap <- as.factor(dapsclae[order(dapsclae)])
      AOIMap3$DAP <- factor(AOIMap3$DAP)
      levels(AOIMap3$DAP) <- kedap
      
      #dap plot
      observeEvent(ttdap(),
                   {
                     
                     output$dapplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "DAP",
                           title = ttdap(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Blues")+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
      ttha <- reactive({
        
        if(unit == "hectare"){
          
          ttha <- paste("Recommended Yield response (t/hectare)")
        }else {
          
          ttha <- paste("Recommended Yield response (t/acre)")
          
        }
      })
      
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      #yield plot
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       sm3
                       
                     })
                   })
      
      #-------------------------------
      #generate downloadable maps
      #-------------------------------
      
      #generate color pallette
      if(unit == "hectare"){
        ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                      "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
        ttz <- "Urea (kg/hectare)"
      }else {
        ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                      "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
        ttz <- "Urea (kg/acre)"
      }
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      require(ggrepel) 
      
      #ggplot urea
      
      ggUrea <- ggplot(AOIMap3) +
        geom_sf(aes(fill=Urea), col="darkgrey") +
        scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        geom_text(data=stateLabel, aes(lon, lat, label=REGION, fontface=2), col='black', size=6)+
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_fancy_orienteering) +
        # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
        #                        style = north_arrow_fancy_orienteering) +
        # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8)) 
      
      
      
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      if(unit == "hectare"){
        NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                     "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
        ttz <- "NPK 17-17-17 (kg/hectare)"
      }else{
        NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                     "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
        ttz <- "NPK 17-17-17 (kg/acre)"
      }
      
      
      mopsclae <- unique(AOIMap3$NPK17_17_17)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
      levels(AOIMap3$NPK17_17_17) <- kev
      
      #ggplot NPK
      ggNPK <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK17_17_17), col="darkgrey") +
        
        scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      
      # DAP color pallette
      DAPPpalette <- brewer.pal(9,"YlGnBu")
      if(unit == "hectare"){
        DAPcols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                     "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
        ttz <- "DAP (kg/hectare)"
      }else{
        DAPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                     "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
        ttz <- "DAP (kg/acre)"
      }
      
      dapsclae <- unique(AOIMap3$DAP)
      kedap <- as.factor(dapsclae[order(dapsclae)])
      AOIMap3$DAP <- factor(AOIMap3$DAP)
      levels(AOIMap3$DAP) <- kedap
      
      #DAP ggplot
      ggDAP <- ggplot(AOIMap3) +
        geom_sf(aes(fill=DAP), col="darkgrey") +
        
        scale_fill_manual(values = DAPcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      
      #generate color pallette # brewer.pal(9,"heat")
      Ydcols <- c("21"="#FF0000FF","20" = "#FF4600FF", "19"= "#FF8B00FF", "18"= "#FFD100FF", "17"= "#E8FF00FF",
                  "16"="#A2FF00FF", "15"= "#5DFF00FF", "14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                  "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                  "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                  "1"= "#FF008BFF", "0"= "#FFFFFF")
      
      if(unit == "hectare"){
        ttz <- "Yield increase (t/hectare)"
      }else{
        ttz <- "Yield increase (t/acre)"
      }
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      #Yield ggplot 
      ggYield <- ggplot(AOIMap3) +
        geom_sf(aes(fill=dY), col="darkgrey") +
        
        scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(ttz) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      #Combine plots together in pdf
      fileName <- paste("maps", ".pdf", sep="")
      pdf(fileName, onefile = TRUE, height = 14, width=12)
      #pdf.options(paper = "a4")
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))   
      grid.text(paste("Planting in", plantMonth, "at", yield_level, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
      print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
      print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      dev.off()
      
      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
      
      #-------------------------------------------------------------------------
      #front page dynamic tmap
      #-------------------------------------------------------------------------
      
      #reactive selection of variable to view
      filt_select <- reactive({
        print(Selection2)
        if (Selection2 == "Urea rate"){
          filt_select <- "Urea rate"
        }else if (Selection2 == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else if (Selection2 == "NPK 17:17:17 rate"){
          filt_select <- "NPK 17:17:17 rate"
        }else{
          filt_select <- "DAP rate"
          
        }
        
      })
      
     
      #show map based on selection of variable but retaining single name
      
      #filter by variable selected and unit for color pallette
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){
          
          ureacols <- reactive({
            
            if(unit == "hectare"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/hectare)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          #reactive legend title
          tturea <- reactive({
            
            if(unit == "hectare"){
              
              tturea <- paste("Recommended urea rate(kg/hectare)")
            }else {
              
              tturea <- paste("Recommended urea rate (kg/acre)")
            }
          })
          
          
          
          ureasclae <- unique(AOIMap3$Urea)
          keU <- as.character(ureasclae[order(ureasclae)])
          AOIMap3$Urea <- factor(AOIMap3$Urea)
          levels(AOIMap3$Urea) <- keU
          
          require(ggrepel)
          library(tmap)
          
          
          #urea
          observeEvent(tturea(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           sm1 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "Urea",
                               title = tturea(),
                               #breaks = c(200, 175, 150, 125,100),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Greens")+
                             tm_text(text = "NAME_2")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 17:17:17 rate"){
          
          ttnpk <- reactive({
            
            if(unit == "hectare"){
              
              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/hectare)")
            }else {
              
              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/acre)")
            }
          })
          
          
          
          mopsclae <- unique(AOIMap3$NPK17_17_17)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
          levels(AOIMap3$NPK17_17_17) <- kev
          
          observeEvent(ttnpk(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK17_17_17",
                               title = ttnpk(),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")
                           
                           sm2
                           
                         }) 
                       })
          
        }else if(filt_select() == "DAP rate"){
          ttdap <- reactive({
            
            if(unit == "hectare"){
              
              ttdap <- paste("Recommended DAP rate (kg/hectare)")
            }else {
              
              ttdap <- paste("Recommended DAP rate (kg/acre)")
            }
          })
          
          
          
          dapsclae <- unique(AOIMap3$DAP)
          kedap <- as.factor(dapsclae[order(dapsclae)])
          AOIMap3$DAP <- factor(AOIMap3$DAP)
          levels(AOIMap3$DAP) <- kedap
          
          observeEvent(ttdap(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "DAP",
                               title = ttdap(),
                               palette = "Blues")+
                             tm_text(text = "NAME_2")
                           sm4
                           
                         }) 
                       })
          
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({
            
            if(unit == "hectare"){
              
              ttha <- paste("Recommended yield increase (t/hectare)")
            }else {
              
              ttha <- paste("Recommended yield increase (t/acre)")
              
            }
          })
          
          
          Ysclae <- unique(AOIMap3$dY)
          keY <- as.factor(Ysclae[order(Ysclae)])
          AOIMap3$dY <- factor(AOIMap3$dY)
          levels(AOIMap3$dY) <- keY
          
          observeEvent(ttha(),
                       {
                         
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")
                           
                           sm3
                           
                         })
                       })
          
          
          
        } 
        
        
      })
      
      
      
      if ( input$use == "Fertilizer Recommendation" & unit == "acre"){
        
        #download acre printable guides
        output$downloadDatafr <- 
          
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(input$use == "Fertilizer Recommendation" & unit == "hectare"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (hectare)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      }
      
      
      output$sidetext <- renderText({
        
        
        # paste0('<span style=\"background-color:', "color", '\ ">',text,' #<span style=\"font-size:8px;font-weight:bold;background-color:white;">',"ent_type",'</span></span>')
        paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "in a field with", yield_level,
              ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months.
              ")
        
      })
      
      
      
      #download maps
      output$downloadData <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
        },
        
        content <- function(file) {
          file.copy("maps.pdf", file)
        },
        contentType = "application/pdf"
      )
      
      #download tables
      output$downloadcsv <- downloadHandler(
        filename <- function() {
          paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
        },
        
        content <- function(file) {
          file.copy("tables.csv", file)
        },
        contentType = "application/csv"
      )
      
      
    })
          
          
        }
  })
    
  observeEvent(input$ok, {
    if(!is.null(input$ok)){
      removeModal()
    }
  })
  
    observeEvent(input$ok, {
    if(input$use == "Scheduled Planting"){
  
      # WLY_CY_FCY1_Region <- readRDS("WLY_CY_FCY1_Region_2020_Average.RDS")
      # #head(WLY_CY_FCY1_Region)
      # 
      # #unique(WLY_CY_FCY1_Region$NAME_1)
      # Zanz <- WLY_CY_FCY1_Region[grep("Zanzibar", WLY_CY_FCY1_Region$NAME_1), ]
      # rest <- WLY_CY_FCY1_Region[-c(grep("Zanzibar", WLY_CY_FCY1_Region$NAME_1)), ]
      # Zanz$NAME_1 <- as.factor(as.character("Zanzibar") )
      # 
      # WLY_CY_FCY1_Region <- rbind( rest,Zanz)
      # 
      # ############################################################################################################
      # ##  TZ: add week nrs
      # ############################################################################################################
      # PHdate <- Planting_HarvestDate(country = "NG")
      # PHdate$Zone <- "TZ"
      # 
      # CY_WLY_coord_TZ <- merge(WLY_CY_FCY1_Region, PHdate[, c("st","weekNr")], by.x="plantingDate", by.y="st")
      # saveRDS(CY_WLY_coord_TZ, "CY_WLY_coord_TZ.RDS")
      
      CY_WLY_coord <- readRDS("CY_WLY_coord_TZ.RDS")
      #head(CY_WLY_coord)
   
      
          removeModal()
      #test parameters     
          # countrySP <- "TZ"
          # plantMonthSP <- "February"
          # 
          # regionSP <- "Kigoma"
          # 
          # unitSP <- "acre"
        observeEvent(input$btn_SP, {
           
                plantMonthSP <- input$plntmthSP
                regionSP <- input$regionSP
                unitSP <- input$unit_locSP
                print(regionSP)
                FCYName <- "FCY_1"
          
                #Filter by LGA
             CY_WLY_coord2 <- CY_WLY_coord[CY_WLY_coord$NAME_1 == regionSP, ]
             
             #' take the mean of WLY and CY from FCY1 by LGA/District within STATE/REGION and planting month and monthly harvest 8 - 15 MAP 
             #' @param ds : output of function addCoord
             CY_wly_average <- as.data.frame(Agg_plantingMonth_SP( ds=CY_WLY_coord2, country="TZ", adminName=regionSP, FCYName="FCY_1", unit=unitSP))
  
              SP_plDate <- CY_wly_average[CY_wly_average$plm == plantMonthSP, ]
              SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2),]
              fname <- paste(regionSP, "_SP_paperBased_ton_", plantMonthSP, ".csv",sep="")
              SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2), ]
        
            
              SP_plDate2 <- SP_plDate[, c("NAME_1", "NAME_2", "Harvest_8","Harvest_9","Harvest_10", "Harvest_11",
                                       "Harvest_12", "Harvest_13", "Harvest_14","Harvest_15","plm" )]
              SP_plDate3 <- SP_plDate2[order(SP_plDate2$NAME_1, SP_plDate2$NAME_2), ]
        

             if(plantMonthSP == "January"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(9:12, 1:4)], "plantingMonth")
            }else if(plantMonthSP == "February"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(10:12, 1:5)], "plantingMonth")
            }else if(plantMonthSP == "March"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(11:12, 1:6)], "plantingMonth")
            }else if(plantMonthSP == "April"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(12, 1:7)], "plantingMonth")
            }else if(plantMonthSP == "May"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(1:8)], "plantingMonth")
            }else if(plantMonthSP == "June"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(2:9)], "plantingMonth")
            }else if(plantMonthSP == "July"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(3:10)], "plantingMonth")
            }else if(plantMonthSP == "August"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(4:11)], "plantingMonth")
            }else if(plantMonthSP == "September"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(5:12)], "plantingMonth")
            }else if(plantMonthSP == "October"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(6:12, 1)], "plantingMonth")
            }else if(plantMonthSP == "November"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(7:12, 1:2)], "plantingMonth")
            }else if(plantMonthSP == "December"){
              colnames(SP_plDate3) <- c("REGION", "DISTRICT", month.name[c(8:12, 1:3)], "plantingMonth")
            }
            
            
              SP_plDate3 <- subset(SP_plDate3, select=-c(plantingMonth))
              cols <- names(SP_plDate3)[3:10]
            
              SP_plDate4 <- SP_plDate3 %>% mutate_at(vars(cols), funs(round(., 1)))
 
              #write.csv(SP_plDate4, fname, row.names = FALSE)
              
            #Render table in reactive envi
            output$tabletext_sp <- renderText({
              
              
              paste("Expected yield in tonnes per", unitSP, "when you plant in", plantMonthSP, "and harvest in ...", sep=" ")
              
            })

            Sys.sleep(2)
            output$SPtable <- renderDT({SP_plDate4},
                                       rownames = FALSE, 
                                       extensions = c('Buttons','FixedColumns'), 
                                       options = list(dom = 'Bfrtip',
                                                      pageLength = nrow(SP_plDate4),
                                                      initComplete = DT::JS(
                                                        "function(settings, json) {",
                                                        "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                        "}"),
                                                      
                                                      buttons = list(
                                                        list(extend = 'excel', 
                                                             filename = paste('AKILIMO SP advice', '_', regionSP, '_', plantMonthSP),
                                                             title = paste("Expected yield in t/acre when you plant in", plantMonthSP, "and harvest in ...", sep=" ")),
                                                        list(extend = 'pdf',
                                                             filename = paste('AKILIMO SP advice', '_', regionSP, '_', plantMonthSP),
                                                             title = paste("Expected yield in t/acre when you plant in", plantMonthSP, "and harvest in ...", sep=" "),
                                                             header = TRUE)
                                                      )
                                                      
                                       )
            ) 
            
           
            #Downloadbale guides 
            if (input$use == "Scheduled Planting" & input$unit_locSP == "acre"){
              #download acre printable guides
              output$downloadDatasp <- downloadHandler(
                filename <- function() {
                  paste("SP Printable guides (acre)",  ".pdf", sep="")
                },
                
                content <- function(file) {
                  file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Acre latest.pdf", file)
                },
                contentType = "application/pdf"
              )
            }else if(input$use == "Scheduled Planting" & input$unit_locSP == "hectare"){
              
              #download hectare printable guides
              output$downloadDatasp <- downloadHandler(
                filename <- function() {
                  paste("SP Printable guides (hectare)",  ".pdf", sep="")
                },
                
                content <- function(file) {
                  file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Hectare latest.pdf", file)
                },
                contentType = "application/pdf"
              )  
              
            } 
          
          

      
            
          })
          
       #    observeEvent(input$btn_SP, {
       #     
       # 
       #      
       # })
    }
  })
      
   
}



#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
