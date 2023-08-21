#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : November 2021 (to include GH)
#' 
#setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH")

#C:\Users\User\Documents\ACAI\paper based\PaperbasedDashboard -v4 - Copy
#C:\Users\User\Documents\ACAI\paper based\PaperBasedAnnex - RW
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

# library(remotes)
# install_github("mtennekes/tmaptools")
# install_github("mtennekes/tmap")

#code that formats the datatable

# data_output <- function(df) {
#   DT::datatable(df, rownames= FALSE, options = list( dom = 'Bfrtip', buttons = c('excel','pdf','print','colvis'),
#                                                      pageLength = nrow(df),  drawCallback = JS(js), initComplete = DT::JS(
#     "function(settings, json) {",
#     "$(this.api().table().header()).css({'font-size': '20px','background-color': '#000000', 'color': '#fff'});",
#     
#     "}") ), list(extend = 'pdf',
#                  filename = 'CurrentTable',
#                  title = paste("My chTitle"),
#                  header = FALSE),
#     extensions = c('Buttons','FixedColumns'))%>%  formatStyle(columns = colnames(.$x$data), `font-size` = '18px')
# }



  
# data_output <- function(df) {
#   
#   datatable(head(iris, 20), options = list(
#     initComplete = JS(
# "function(settings, json) {",
# "$(this.api().table().header()).css({'font-size': '5px', 'background-color': '#c2d1f0', 'color': '#fff'});",
# "}"))) %>%  formatStyle(columns = colnames(.$x$data), `font-size` = '12px')
# }
### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
      shinyalert("Paper based Annex", "This tool contains tables and maps with advice on application rates of urea,
                         DAP & NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                         conditions and the time of planting.
                         <br>
                         <br>
                         <br>
                         

                         Please wait....
                  <br>
                         <br>
                         (This window will automatically close when country data is loaded)
                 
                 
                 ", type = "info", timer = 14500, size = 'm',
                 closeOnClickOutside = FALSE,
                 closeOnEsc = FALSE,
                 animation = TRUE,
                 html = TRUE,

                 showConfirmButton = FALSE,
                 showCancelButton = FALSE,
                 confirmButtonText = "OK",
                 confirmButtonCol = "#AEDEF4")
 
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
      text = "Please wait while the map loads..."
    )
    Sys.sleep(6)
    remove_modal_spinner()
  })
  
  #.............................................................................
  #render select input options
  #.............................................................................
  output$country <- renderUI({

    pickerInput("country", "Select Country:",
                choices = c("Nigeria", "Tanzania", "Rwanda", "Ghana"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
      })
#})

  observeEvent(input$country, {
    if(input$country == "Nigeria" | input$country == "Tanzania" | input$country == "Rwanda" | input$country == "Ghana")  {
 
      output$usecase <- renderUI({
       
        pickerInput("usecase", "Select use case",
                    choices = c("Fertilizer Recommendation", "Scheduled Planting"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1),
                    )
            })
    }
  })
    
 
  output$region <- renderUI({
        
        pickerInput("region", "Select region(s)", choices = c("Geita", "Kagera","Kigoma","Lindi", "Mara", "Mtwara", "Mwanza","Pwani", "Shinyanga",  
                                                              "Simiyu", "Tanga","Zanzibar South and Central"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
   
      
  output$lga_Groups <- renderUI({
        
        pickerInput("lga_Groups", "Select state",
                    choices = c("Abia", "Akwa Ibom","Anambra", "Benue", "Cross River", "Delta", "Ebonyi","Edo", "Ekiti", 
                                "Enugu","Imo", "Kogi", "Kwara","Ogun", "Ondo", "Osun",  "Oyo","Taraba"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
      
  output$state <- renderUI({

    pickerInput("state", "Select state",
                choices = c("Amajyepfo", "Amajyaruguru","Umujyi wa Kigali", "Iburengerazuba", "Iburasirazuba"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
  output$reg_ghana <- renderUI({
    
    pickerInput("reg_ghana", "Select region",
                choices = c("Savannah", "Ashanti","Ahafo", "Volta",  "Central",  "Eastern", "Bono East", "Bono"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
  observeEvent(input$usecase, {
    
    if(!is.null(input$region) | !is.null(input$lga_Groups) | !is.null(input$state) | !is.null(input$reg_ghana))  {
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
    
    pickerInput("selection", "Select variable to view",
                              choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
                              selected = NULL,
                              multiple = TRUE,
                              options = pickerOptions(maxOptions = 1))
  })
  
  output$selection2 <- renderUI({
    
    pickerInput("selection2", "Select variable to view",
                                choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),
                                selected = NULL,
                                multiple = TRUE,
                                options = pickerOptions(maxOptions = 1))
  })

  output$selection3 <- renderUI({
    
    pickerInput("selection3", "Select variable to view",
                choices = c("NPK 15:20:20 rate", "NPK 11:22:21 rate", "NPK 25:10:10 rate", "NPK 12:30:17 rate", "Expected yield response", "Urea rate"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
    }
  })

  observeEvent(input$usecase, {
    if(!is.null(input$usecase))  {
      
      output$unit_loc <- renderUI({
        
        selectInput("unit_loc", "Select unit of land",
                    choices = c("acre", "ha"))
                  
        
        
        
      })
    }
  }) 
  
  observeEvent(input$usecase, {
    if(!is.null(input$usecase))  {
      
      output$unit_loc_rw <- renderUI({
        
        selectInput("unit_loc_rw", "Select unit of land",
                    choices = c("are", "ha"))
        
        
        
        
      })
    }
  }) 
  
  observeEvent(input$unit_loc, {
    if(!is.null(input$unit_loc))  {
      output$FCY_ha <- renderUI({

        selectInput("FCY_ha", "Select Your Current Yield (Tonnes)",
                                  choices = c("0-7.5 t/ha", "7.5-15 t/ha", "15-22.5 t/ha", "22.5-30 t/ha", ">30 t/ha",  ""),
                    selected = "")            
      })
    

      output$FCY_acre <- renderUI({

        selectInput("FCY_acre", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/acre", "3-6 t/acre", "6-9 t/acre", "9-12 t/acre", ">12 t/acre", ""),
                    selected = "")
      })
    }
      
    })  
  
  observeEvent(input$unit_loc_rw, {
    if(!is.null(input$unit_loc_rw))  {
      output$FCY_ha_rw <- renderUI({
        
        selectInput("FCY_ha_rw", "Select Your Current Yield (Tonnes)",
                    choices = c("0-7.5 t/ha", "7.5-15 t/ha", "15-22.5 t/ha", "22.5-30 t/ha", ">30 t/ha",  ""),
                    selected = "")            
      })
      
      
      output$FCY_are_rw <- renderUI({
        
        selectInput("FCY_are_rw", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/are", "3-6 t/are", "6-9 t/are", "9-12 t/are", ">12 t/are", ""),
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
 
       output$NPK151515Price <- renderUI({
    
    textInput("NPK151515Price", "Cost of NPK:15:15:15 per 50Kg bag")
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
    if(input$costs == "Yes")  {
      
      output$NPK112221Price <- renderUI({
        
        textInput("NPK112221Price", "Cost of NPK:11:22:21 per 50Kg bag")
      })
      
      output$NPK251010Price <- renderUI({
        
        textInput("NPK251010Price", "Cost of NPK:25:10:10 per 50Kg bag")
      })
      
      output$NPK152020Price <- renderUI({
        
        textInput("NPK152020Price", "Cost of NPK:15:20:20 per 50Kg bag")
      })
      
      output$NPK123017Price <- renderUI({
        
        textInput("NPK123017Price", "Cost of NPK:12:30:17 per 50Kg bag")
      })
    
      
    }
  })
  
  
  #ADD GHANA FERT HERE
  
  
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
  
  
 #.................................................................................................................
  

  
        #######################################################################################
      ## Read the GIS layers
      #######################################################################################
  
 
      TownsNG <- readOGR(dsn = ".", layer = "Places_towns")
    
      RiversNG <- readOGR(dsn = ".", layer = "Rivers")
      
 
      boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")
      
       
      ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")
      
    
      boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
      

      tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")
      
      boundaryRW <- readOGR(dsn=getwd(), layer="gadm36_RWA_1")
      
      
      rwState <- readOGR(dsn=getwd(), layer="gadm36_RWA_2")
    
      boundaryGH <- readOGR(dsn=getwd(), layer="gha_admbnda_adm1_gss_20210308")
      ghRegions <- readOGR(dsn=getwd(), layer="gha_admbnda_adm2_gss_20210308")
      
        
      
      ###################################################################################################
      ## NG fertilizer recom for FCY 1:5
      ###################################################################################################
      FR_NG_FCY1 <- readRDS("FRrecom_lga_level1_NG_2020.RDS")
      FR_NG_FCY2 <- readRDS("FRrecom_lga_level2_NG_2020.RDS")
      FR_NG_FCY3 <- readRDS("FRrecom_lga_level3_NG_2020.RDS")
      FR_NG_FCY4 <- readRDS("FRrecom_lga_level4_NG_2020.RDS")
      FR_NG_FCY5 <- readRDS("FRrecom_lga_level5_NG_2020.RDS")
      
   
      ###########################################################################
      ##  TZ fertilizer recom for FCY 1:5
      ###########################################################################
      FR_TZ_FCY1 <- readRDS("FRrecom_lga_level1_TZ_2020.RDS")
      FR_TZ_FCY2 <- readRDS("FRrecom_lga_level2_TZ_2020.RDS")
      FR_TZ_FCY3 <- readRDS("FRrecom_lga_level3_TZ_2020.RDS")
      FR_TZ_FCY4 <- readRDS("FRrecom_lga_level4_TZ_2020.RDS")
      FR_TZ_FCY5 <- readRDS("FRrecom_lga_level5_TZ_2020.RDS")
      

      ###########################################################################
      ## RW fertilizer recom for FCY 1:5
      ###########################################################################
      FR_RW_FCY <- readRDS("RW_CassavaPaperBased.RDS")
      FR_RW_FCY1 <- FR_RW_FCY[FR_RW_FCY$FCY == "level1", ]
      FR_RW_FCY2 <- FR_RW_FCY[FR_RW_FCY$FCY == "level2", ]
      FR_RW_FCY3 <- FR_RW_FCY[FR_RW_FCY$FCY == "level3", ]
      FR_RW_FCY4 <- FR_RW_FCY[FR_RW_FCY$FCY == "level4", ]
      FR_RW_FCY5 <- FR_RW_FCY[FR_RW_FCY$FCY == "level5", ]
    
      
      ###########################################################################
      ## GH fertilizer recom for FCY 1:5
      ###########################################################################
      
      FR_GH_FCY <- readRDS("GH_FR_CassavaPaperBased.RDS")
      FR_GH_FCY1 <- FR_GH_FCY[FR_GH_FCY$FCY == "level1", ]
      FR_GH_FCY2 <- FR_GH_FCY[FR_GH_FCY$FCY == "level2", ]
      FR_GH_FCY3 <- FR_GH_FCY[FR_GH_FCY$FCY == "level3", ]
      FR_GH_FCY4 <- FR_GH_FCY[FR_GH_FCY$FCY == "level4", ]
      FR_GH_FCY5 <- FR_GH_FCY[FR_GH_FCY$FCY == "level5", ]
      
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
        
        if(country=="NG"){
          ds$rateUrea <- ds$urea
          ds$rateNPK151515 <- ds$NPK15_15_15
        }else{
          ds$rateUrea <- ds$urea
          ds$rateNPK171717 <- ds$NPK17_17_17
          ds$rateDAP <- ds$DAP
        }
        return(ds)
        }
 
      FR_NG_FCY1_plm <- addplm(ds=FR_NG_FCY1, country = "NG") ## NG if user current yield is level 1
      FR_NG_FCY2_plm <- addplm(ds=FR_NG_FCY2, country = "NG") ## NG if user current yield is level 2
      FR_NG_FCY3_plm <- addplm(ds=FR_NG_FCY3, country = "NG") ## NG if user current yield is level 3
      FR_NG_FCY4_plm <- addplm(ds=FR_NG_FCY4, country = "NG") ## NG if user current yield is level 4
      FR_NG_FCY5_plm <- addplm(ds=FR_NG_FCY5, country = "NG") ## NG if user current yield is level 5
      
      
      FR_TZ_FCY1_plm <- addplm(ds=FR_TZ_FCY1, country = "TZ") ## TZ if user current yield is level 1
      FR_TZ_FCY2_plm <- addplm(ds=FR_TZ_FCY2, country = "TZ") ## TZ if user current yield is level 2
      FR_TZ_FCY3_plm <- addplm(ds=FR_TZ_FCY3, country = "TZ") ## TZ if user current yield is level 3
      FR_TZ_FCY4_plm <- addplm(ds=FR_TZ_FCY4, country = "TZ") ## TZ if user current yield is level 4
      FR_TZ_FCY5_plm <- addplm(ds=FR_TZ_FCY5, country = "TZ") ## TZ if user current yield is level 5
      
      
      FR_RW_FCY1_plm <- addplm(ds=FR_RW_FCY1, country = "RW") ## RW if user current yield is level 1
      FR_RW_FCY2_plm <- addplm(ds=FR_RW_FCY2, country = "RW") ## RW if user current yield is level 2
      FR_RW_FCY3_plm <- addplm(ds=FR_RW_FCY3, country = "RW") ## RW if user current yield is level 3
      FR_RW_FCY4_plm <- addplm(ds=FR_RW_FCY4, country = "RW") ## RW if user current yield is level 4
      FR_RW_FCY5_plm <- addplm(ds=FR_RW_FCY5, country = "RW") ## RW if user current yield is level 5
      
      FR_GH_FCY1_plm <- addplm(ds=FR_GH_FCY1, country = "GH") ## GH if user current yield is level 1
      FR_GH_FCY2_plm <- addplm(ds=FR_GH_FCY2, country = "GH") ## GH if user current yield is level 2
      FR_GH_FCY3_plm <- addplm(ds=FR_GH_FCY3, country = "GH") ## GH if user current yield is level 3
      FR_GH_FCY4_plm <- addplm(ds=FR_GH_FCY4, country = "GH") ## GH if user current yield is level 4
      FR_GH_FCY5_plm <- addplm(ds=FR_GH_FCY5, country = "GH") ## GH if user current yield is level 5
      ###########################################################################
      ## select FCY and read the corresponding file 
      ## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
      ###########################################################################
      
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 
      ## Determine platform type and set working directory accordingly
   
      
      
    observeEvent(input$btn_go, {
    
    #define reactive values
    country <- input$country
    FCY_ha <- input$FCY_ha
    print(FCY_ha)
    FCY_acre <- input$FCY_acre
    FCY_are_rw <- input$FCY_are_rw
    FCY_ha_rw <- input$FCY_ha_rw
    print(FCY_acre)
    Selection <- input$selection
    Selection2 <- input$selection2
    Selection3 <- input$selection3
    usecase <- input$usecase
    plantMonth <- input$plntmth
    state <- input$state
    region <- input$region
    lga_Groups <- input$lga_Groups
    reg_ghana <- input$reg_ghana
    plantMonth <- input$plntmth
    cities <- input$city
    unit <- input$unit_loc
    unit_rw <- input$unit_loc_rw
    UreaPrice <- as.numeric(input$UreaPrice)
    DAPPrice <- as.numeric(input$DAPPrice)
    NPK151515Price <- as.numeric(input$NPK151515Price)
    NPK171717Price <- as.numeric(input$NPK171717Price)
    
    
    NPK112221Price <- as.numeric(input$NPK112221Price)
    NPK251010Price <- as.numeric(input$NPK251010Price)
    NPK152020Price <- as.numeric(input$NPK152020Price)
    NPK123017Price <- as.numeric(input$NPK123017Price)
   
    CassavaPrice <- as.numeric(input$CassavaPrice)
    lgaGroups <- input$lga_Groups
    costs <- input$costs

    print(unit)
    print(unit_rw)
    print(plantMonth)
    
    #specify yield categories
    if (country != 'Rwanda'){
    if(unit == 'ha'){
    yield_level <- ifelse( FCY_ha == "0-7.5 t/ha", "a low yield level",
                          ifelse( FCY_ha == "7.5-15 t/ha","a normal yield level",
                                 ifelse( FCY_ha == "15-22.5 t/ha","a medium yield level", 
                                        ifelse( FCY_ha == "22.5-30 t/ha","a high yield level",
                                               ifelse( FCY_ha == ">30 t/ha","a very high yield level"
                                                       )))))
    }else if(unit == 'acre'){ 
      yield_level <- ifelse( FCY_acre == "0-3 t/acre","a low yield level",
                                                             ifelse( FCY_acre == "3-6 t/acre","a normal yield level",
                                                                    ifelse( FCY_acre == "6-9 t/acre","a medium yield level",
                                                                           ifelse( FCY_acre == "9-12 t/acre","a high yield level",
                                                                                  ifelse( FCY_acre == ">12 t/acre","a very high yield level")
                                                             ))))
        }
    } else if (country == 'Rwanda'){
    
    #MEKLIT HERE
   
    if(unit_rw == 'are'){ 
      yield_level <- ifelse( FCY_are_rw == "0-7.5 t/are","a low yield level",
                             ifelse( FCY_are_rw == "3-6 t/are","a normal yield level",
                                     ifelse( FCY_are_rw == "6-9 t/are","a medium yield level",
                                             ifelse( FCY_are_rw == "9-12 t/are","a high yield level",
                                                     ifelse( FCY_are_rw == ">12 t/are","a very high yield level")
                                             ))))
    }else if(unit_rw == 'ha'){ 
      yield_level <- ifelse( FCY_ha_rw == "0-7.5 t/ha","a low yield level",
                             ifelse( FCY_ha_rw == "3-6 t/ha","a normal yield level",
                                     ifelse( FCY_ha_rw == "6-9 t/ha","a medium yield level",
                                             ifelse( FCY_ha_rw == "9-12 t/ha","a high yield level",
                                                     ifelse( FCY_ha_rw == ">12 t/ha","a very high yield level")
                                             ))))
    }
   
    }
    
    print(yield_level)
  
    #lga_Groups = "Abia"    
    if(country == 'Nigeria'){
    lgaGroups <- input$lga_Groups
    }else if (country == "Tanzania"){
      lgaGroups <- input$region
    }else if(country == "Rwanda"){
      lgaGroups <- input$state
    }else if(country == "Ghana"){
      lgaGroups <- input$reg_ghana
    }
    

    
    if(country == 'Nigeria'){
      lgaGroups2 <- input$lga_Groups
    }else if (country == "Tanzania"){
      lgaGroups2 <- input$region
    }else if(country == "Rwanda"){
      lgaGroups2 <- input$state
    }else if(country == "Ghana"){
      lgaGroups2 <- input$reg_ghana
    }
    
    
    if(country == 'Nigeria'){
      if (unit == "ha"){
        FCY <- FCY_ha
        
        if(FCY == "7.5-15 t/ha" ){
          ds <- FR_NG_FCY2_plm
        }else if(FCY == "0-7.5 t/ha" ){
          ds <- FR_NG_FCY1_plm
        }else if(FCY == "15-22.5 t/ha" ){
          ds <- FR_NG_FCY3_plm
        }else if(FCY == "22.5-30 t/ha"){
          ds <- FR_NG_FCY4_plm
        }else if(FCY == ">30 t/ha" ){
          ds <- FR_NG_FCY5_plm
        }
      }else if(unit == "acre"){
        FCY <- FCY_acre
        
        if(FCY == "3-6 t/acre" ){
          ds <- FR_NG_FCY2_plm
        }else if(FCY == "0-3 t/acre" ){
          ds <- FR_NG_FCY1_plm
        }else if(FCY == "6-9 t/acre" ){
          ds <- FR_NG_FCY3_plm
        }else if(FCY == "9-12 t/acre" ){
          ds <- FR_NG_FCY4_plm
        }else if(FCY == ">12 t/acre" ){
          ds <- FR_NG_FCY5_plm
          
        }
      }
    
      ####################################################################################################### 
     
      #Subset by state for every filter option presented by users
      Oyo <- droplevels(ds[ds$STATE == "Oyo", ])
      Oyolabel <- data.frame(state= c("Oyo"), lon=c(3.3), lat=c(9))
      
      Ogun <- droplevels(ds[ds$STATE == "Ogun", ])
      Ogunlabel <- data.frame(state= c("Ogun"), lon=c(3.4), lat=c(7.65))
      
      Kogi <- droplevels(ds[ds$STATE == "Kogi", ])
      Kogilabel <- data.frame(state= c("Kogi"), lon=c(6.63), lat=c(8.56))
      
      Kwara <- droplevels(ds[ds$STATE %in% c("Kwara"), ])
      Kwaralabel <- data.frame(state= c( "Kwara"), lon=c(4.9), lat=c(9.5))
      
      Taraba <- droplevels(ds[ds$STATE %in% c("Taraba"), ])
      Tarabalabel <- data.frame(state= c( "Taraba"), lon=c(10.2), lat=c(9.05))
      
      CrossRiver <- droplevels(ds[ds$STATE %in% c("Cross River"), ])
      Crossriver_label <- data.frame(state= c("Cross River"), lon=c(8), lat=c(8.2) )
      
      Benue <- droplevels(ds[ds$STATE %in% c("Benue"), ])
      Benue_label <- data.frame(state= c("Benue"), lon=c(9.5), lat=c(8))
      
      Edo <- droplevels(ds[ds$STATE %in% c("Edo"), ])
      Edolabel <- data.frame(state= c("Edo"), lon=c(5.3), lat=c(7))
      
      Delta <- droplevels(ds[ds$STATE %in% c("Delta"), ])
      Deltalabel <- data.frame(state= c("Delta"), lon=c(6), lat=c(5))
      
      Akwa_Ibom <- droplevels(ds[ds$STATE %in% c("Akwa Ibom"), ])
      Akwa_Ibomlabel <- data.frame(state= c( "Akwa Ibom"), lon=c(8), lat=c(5.45))
      
      Imo <- droplevels(ds[ds$STATE %in% c("Imo"), ])
      Imolabel <- data.frame(state= c( "Imo"), lon=c(6.9), lat=c(5.9))
      
      Abia <- droplevels(ds[ds$STATE %in% c("Abia"), ])
      Abialabel <- data.frame(state= c( "Abia"), lon=c(7.7), lat=c(5.9))
      
      
      Ondo <- droplevels(ds[ds$STATE %in% c("Ondo"), ])
      Ondolabel <- data.frame(state= c( "Ondo"), lon=c(5.3), lat=c(6.6))
      
      Ekiti <- droplevels(ds[ds$STATE %in% c("Ekiti"), ])
      Ekitilabel <- data.frame(state= c( "Ekiti"), lon=c(5.3), lat=c(8.1))
      
      Osun <- droplevels(ds[ds$STATE == "Osun", ])
      Osunlabel <- data.frame(state= c("Osun"), lon=c(4.2), lat=c(8.05))
      
       
      Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
      Anambralabel <- data.frame(state= c( "Anambra"), lon=c(7.15), lat=c(6.45))
      
      Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
      Ebonyilabel <- data.frame(state= c( "Ebonyi"), lon=c(7.83), lat=c(6.67))
      
      Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
      Anambra_label <- data.frame(state= c("Anambra"), lon=c(6.7), lat=c(5.9))
      
      Enugu <- droplevels(ds[ds$STATE %in% c("Enugu"), ])
      Enugulabel <- data.frame(state= c("Enugu"), lon=c(7), lat=c(7.1))
      
      Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
      Ebonyilabel <- data.frame(state= c("Ebonyi"), lon=c(8.25), lat=c(6.9))
      
      #specify other key values for getting recommendations  
      
      if(lgaGroups == "Benue"){
        cities <- c("Makurdi")
        LGApoints <- Benue 
        stateLabel <- Benue_label
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Cross River"){
        cities <- c("Calabar")
        LGApoints <- Benue 
        stateLabel <- Benue_label
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Enugu"){
        cities <- c("Enugu")
        LGApoints <- Enugu 
        stateLabel <- Enugulabel
        textangle <- 0 
        couple = "One"
      }else if(lgaGroups =="Delta"){
        cities = c("Asaba")
        LGApoints <- Delta 
        stateLabel <- Deltalabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Edo"){
        cities = c("Benin City")
        LGApoints  <-  Edo 
        stateLabel  <-  Edolabel
        textangle <- 0 
        couple  <-  "One"
      }  else if(lgaGroups == "Imo"){
        cities = c("Owerri")
        LGApoints  <-  Imo 
        stateLabel  <-  Imolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Abia"){
        cities = c("Umuahia")
        LGApoints  <-  Abia 
        stateLabel  <-  Abialabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Akwa Ibom"){
        cities = c("Uyo")
        LGApoints  <-  Akwa_Ibom 
        stateLabel  <-  Akwa_Ibomlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ekiti"){
        cities = c("Ado Ekiti")
        LGApoints  <-  Ekiti 
        stateLabel  <-  Ekitilabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ondo"){ 
        cities = c("Akure")
        LGApoints  <-  Ondo
        stateLabel  <-  Ondolabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Osun"){ 
        cities = c("Osogbo")
        LGApoints  <-  Osun
        stateLabel  <-  Osunlabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Anambra"){ 
        cities = c("Awka")
        LGApoints  <-  Anambra
        stateLabel  <-  Anambralabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Ebonyi"){ 
        cities = c("Ebonyi")
        LGApoints  <-  Ebonyi
        stateLabel  <-  Ebonyilabel
        textangle <- 0 
        couple  <-  "One"
      }else if(lgaGroups == "Taraba"){ 
        cities = "Taraba"
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kogi"){ 
        cities = "Kogi"
        LGApoints  <-  Taraba
        stateLabel  <-  Tarabalabel
        textangle <- 0 
        couple  <-  "One"
        
      }else if(lgaGroups == "Kwara"){
        cities = "Kwara"
        LGApoints  <-  Kwara
        stateLabel  <-  Kwaralabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else if(lgaGroups == "Oyo"){ 
        cities = "Oyo"
        LGApoints  <-  Oyo
        stateLabel  <-  Oyolabel
        textangle <- 0 
        couple  <-  "One" 
        
      }else if(lgaGroups == "Ogun"){ 
        cities = "Abeokuta"
        LGApoints == Ogun
        stateLabel  <-  Ogunlabel
        textangle <- 0 
        couple  <-  "One"
        
      }
      
      
      #filter by month and couple and state
      #plantMonth = "January"
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
      
      if(couple == "Two"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
      }
      
      if(couple == "Three"){
        lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
      }
      
      plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
      
      #incorporate GIS layers
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryNG, NAME_1 %in% AOI ) 
      
      AOIMap <- subset(ngstate, NAME_1 %in% AOI )
      AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("STATE","LGA","long","lat"  )
      LGAnames <- LGAnames[!LGAnames$LGA %in% c("IbadanNorth-West","IbadanNorth-East","IbadanSouth-West", "IbadanSouth-East"),]
      LGAnames$LGA <- gsub("Egbado /", "", LGAnames$LGA )
      
      
      crop_ngstate <- subset(ngstate, NAME_1 %in% AOI )
      towns <- as.data.frame(TownsNG)
      towns <- towns[towns$name %in% cities & towns$fclass %in% c("town", "city"),]
      crop_RiversNG <-  crop(RiversNG, extent(crop_ngstate))
      crop_RiversNG <- crop_RiversNG[crop_RiversNG$fclass == "river", ]
      
      
      LGAaverage <- ddply(plotData, .(LGA, STATE), summarize,
                          LGAUrea = round(mean(rateUrea), digits=0),
                          LGANPK151515 = round(mean(rateNPK151515), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      dss <- LGAaverage
      dss$LGAUrea <- dss$LGAUrea / 2.47105
      dss$LGANPK151515 <- dss$LGANPK151515 / 2.47105
      dss$LGAdY <- dss$LGAdY / 2.47105
      
      if(unit == 'acre'){
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("LGA", "STATE"))
      
      if(unit == "ha"){
        plotData$Urea <- round(plotData$LGAUrea/25)*25
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/50)*50
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK15_15_15 <- round(plotData$LGANPK151515/20)*20
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      #csv tables naming
      fileNameCsv <- paste("tables", ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("LGA", "Urea", "NPK15_15_15","dY", "LGAdY")]),by.x="NAME_2" ,by.y="LGA")
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
      
   
      
      #generate table
      Currency <- "Naira"
      tt <- unique(as.data.frame(plotData[, c("STATE","LGA", "Urea", "NPK15_15_15", "LGAdY", "month")]))
      tt$LGAdY <- round(tt$LGAdY, digits = 1)
      tt <- tt[order(tt$STATE, tt$LGA, tt$month), ]
      
      tt2 <- dplyr::select(tt, c(STATE, LGA, Urea, NPK15_15_15,LGAdY))
      colnames(tt2) <- c("State","LGA", "Urea (kg/ha)", "NPK 15:15:15 (kg/ha)", "Expected yield increase (t)")
     
      
      #subset by cost information in a reactive environment
     
      if(costs == "No"){
        
        output$tabletext_naira <- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
          
        })
        
        output$mytable <- renderDT({tt2},
                                        rownames = FALSE, 
                                        extensions = c('Buttons','FixedColumns'), 
                                        options = list(dom = 'Bfrtip',
                                                       pageLength = nrow(tt2),
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
        
        # data_output <- function(df) {
        #   DT::datatable(df, rownames= FALSE, options = list( dom = 'Bfrtip', buttons = c('excel','pdf','print','colvis'), pageLength = nrow(df), initComplete = DT::JS(
        #     "function(settings, json) {",
        #     "$(this.api().table().header()).css({'background-color': '#369BE9', 'color': '#fff'});",
        #     "}") ), list(extend = 'pdf',
        #                  filename = 'CurrentTable',
        #                  title = paste("My chTitle"),
        #                  header = FALSE),
        #     extensions = c('Buttons','FixedColumns'))
        # }
      
      }else if (costs == "Yes"){
      #tt$totalCost = tt[(UreaPrice * UreaRate) + (NPK171717Price * NPK171717Rate)+(DAPPrice * DAPRate)]
    
      tt_dataframe <- reactive({
        
        df_tt <- data.frame(UreaPrice=input$UreaPrice,NPK151515Price=input$NPK151515Price,CassavaPrice=input$CassavaPrice,
                            STATE=input$lga_Groups)
        
        return(df_tt)
      })
      
  
      #incorporate price information in csv table for display and download by the user
      tt_merge <- merge(tt, tt_dataframe(),by="STATE")
      
      tt_merge$totalSalePrice = as.numeric(tt_merge$LGAdY)  * as.numeric(tt_merge$CassavaPrice)
      tt_merge$totalCost = (as.numeric(tt_merge$UreaPrice)/50 * as.numeric(tt_merge$Urea)) + (as.numeric(tt_merge$NPK151515Price)/50 * as.numeric(tt_merge$NPK15_15_15))
      tt_merge$NetRevenue = as.numeric(tt_merge$totalSalePrice) - as.numeric(tt_merge$totalCost)
      head(tt_merge)
      #tt_merge2 <- dplyr::select(tt_merge, c(STATE, LGA,month, Urea, NPK15_15_15,LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
      
      tt_merge2 <- dplyr::select(tt_merge, c(STATE, LGA, Urea, NPK15_15_15,LGAdY, totalSalePrice, totalCost, NetRevenue))
      colnames(tt_merge2) <- c("State","LGA", "Urea (kg/ha)", "NPK 15:15:15 (kg/ha)", "Expected yield increase (t)", 
                                "Total sale (Naira)", "Fertilizer cost (Naira)", "Profit (Naira)")
      output$tabletext_naira <- renderText({
        
        
        paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
        
      })
      
      output$mytable2 <- renderDT({tt_merge2},
                                 rownames = FALSE, 
                                 extensions = c('Buttons','FixedColumns'), 
                                 options = list(dom = 'Bfrtip',
                                                pageLength = nrow(tt_merge2),
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
      # output$mytable2 = DT::renderDataTable({
      #   data_output(tt_merge2)
      # }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
      # )
    }
      
      # --------------------------------------------------------------------------
      #side by side maps
      # --------------------------------------------------------------------------
      AOIMap3 <- st_as_sf(AOIMap2)
      #urea plot
      ################################################
      
      #reactive  title based on unit of land
     
      filt_select <- reactive({
        print(Selection)
        if (Selection == "Urea rate"){
          
          filt_select <- "Urea rate"
        }else if (Selection == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else{
          filt_select <- "NPK 15:15:15 rate"
        }
        
      })
      
      tturea <- reactive({
        
        if(unit == "ha"){
          
          tturea <- paste("Recommended urea rate (kg/ha)")
        }else {
          
          tturea <- paste("Recommended urea rate (kg/acre)")
        }
      })
       
      ureasclae <- unique(AOIMap3$Urea)
      keU <- as.character(ureasclae[order(ureasclae)])
      AOIMap3$Urea <- factor(AOIMap3$Urea)
      levels(AOIMap3$Urea) <- keU
      
      #tmap output using reactive values
      require(ggrepel)
      library(tmap)
   
      observeEvent(tturea(),
                   {
                     
                     output$ureaplot2 <- renderTmap({
                       
                       
                       sm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "Urea",
                           title = tturea(),
                           palette = "Greens")+
                         tm_text(text = "NAME_2")
                       sm1
                       
                       
                       
                     })
                   })
      #NPK plot
      ############################################################################   
    
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk <- reactive({
        
        if(unit == "ha"){
          
          
          ttnpk <- paste("Recommended NPK 15:15:15 rate (kg/ha)")
        }else {
          
          ttnpk <- paste("Recommended NPK 15:15:15 rate (kg/acre)") 
        }
      })
      
      
      
      mopsclae <- unique(AOIMap3$NPK15_15_15)
      kev <- as.character(mopsclae[order(mopsclae)])
      AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
      levels(AOIMap3$NPK15_15_15) <- kev
      
      #tmap output using reactive values 
      observeEvent(ttnpk(),
                   {
                     
                     output$npkplot <- renderTmap({
                      
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK15_15_15",
                           title = ttnpk(),
                    
                           palette = "YlOrBr")+
                         tm_text(text = "NAME_2")
            
                         
                     })
                   })
      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
       ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Recommended yield response (t/ha)")
        }else {
          
          ttha <- paste("Recommended yield response (t/acre)")
        }
      })
      
    
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      #tmap output using reactive values
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
                       
                       
                       
                     })
                   })
      

      
      #-------------------------------------------------------------------------
      #front page dynamic tmap
      #this map changes based on input at the filter level
      #-------------------------------------------------------------------------
      
      #reactive selection of variable to view
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Recommended urea rate(kg/ha)")
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
          
          #tmap output using reactive values 
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
                             tm_legend(legend.position = c("right", "top"))+
                             tm_text(text = "NAME_2")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 15:15:15 rate"){
          
          ttnpk <- reactive({
            
            if(unit == "ha"){
              
              ttnpk <- paste("NPK 15:15:15 rate (kg/ha)")
            }else {
              
              ttnpk <- paste("NPK 15:15:15 rate (kg/acre)")
            }
          })
          
          mopsclae <- unique(AOIMap3$NPK15_15_15)
          kev <- as.character(mopsclae[order(mopsclae)])
          AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
          levels(AOIMap3$NPK15_15_15) <- kev
          
          #tmap output using reactive values 
          #npk
          observeEvent(ttnpk(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK15_15_15",
                               title = ttnpk(),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")
                         
                           sm2
                           
                         }) 
                       })
          
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({
            
            if(unit == "ha"){
              
              ttha <- paste("Recommended yield response (t/ha)")
            }else {
              
              ttha <- paste("Recommended yield response (t/acre)")
            }
          })
          
          
          Ysclae <- unique(AOIMap3$dY)
          keY <- as.factor(Ysclae[order(Ysclae)])
          AOIMap3$dY <- factor(AOIMap3$dY)
          levels(AOIMap3$dY) <- keY
          
          #tmap output using reactive values 
          #yield
          observeEvent(ttha(),
                       {
                         
                         
                         output$tmapplot <- renderTmap({
                           
                           
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
          
        }
        
        
        #generate static maps
 
       
        #color pallette
         if(unit == "ha"){
          ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                        "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
          tt <- "Urea (kg/ha)"
        }else {
          ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                        "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
          tt <- "Urea (kg/acre)"
        }
        ureasclae <- unique(AOIMap3$Urea)
        keU <- as.character(ureasclae[order(ureasclae)])
        AOIMap3$Urea <- factor(AOIMap3$Urea)
        levels(AOIMap3$Urea) <- keU
        
        require(ggrepel) 
        
        #ggplot urea
        ggUrea <- ggplot(AOIMap3) +
          geom_sf(aes(fill=Urea), col="grey30") +
          scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          annotation_scale(location = "br", width_hint = 0.3, line_width = 0.4) +
          annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                 style = north_arrow_fancy_orienteering) +
          # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
          #                        style = north_arrow_fancy_orienteering) +
          # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8)) 
        
        
        ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
        if(unit == "ha"){
          NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                       "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
          tt <- "NPK 15-15-15 (kg/ha)"
        }else{
          NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                       "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
          tt <- "NPK 15-15-15 (kg/acre)"
        }
        
        
        mopsclae <- unique(AOIMap3$NPK15_15_15)
        kev <- as.character(mopsclae[order(mopsclae)])
        AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
        levels(AOIMap3$NPK15_15_15) <- kev
        
        
        #npk ggplot
        require(plotly)
        ggNPK <- ggplot(AOIMap3) +
          geom_sf(aes(fill=NPK15_15_15), col="grey30") +
          scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        #yield color pallette
        if(unit == "ha"){
          tt <- "Yield increase (t/ha)"
          Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
                      "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
                      "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
        }else{
          tt <- "Yield increase (t/acre)"
          Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
                      "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
                      "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
        }
        
        Ysclae <- unique(AOIMap3$dY)
        keY <- as.factor(Ysclae[order(Ysclae)])
        AOIMap3$dY <- factor(AOIMap3$dY)
        levels(AOIMap3$dY) <- keY
        
        #yield ggplot
        ggYield <- ggplot(AOIMap3) +
          geom_sf(aes(fill=dY), col="grey30") +
          scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
          #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
          #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
          geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
          geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
        
        #put the maps together in pdf format
        fileName <- paste("maps", ".pdf", sep="")
        pdf(fileName, onefile = TRUE, height = 14, width=12)
        #pdf.options(paper = "a4")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5,0.8), "null"))))   
        grid.text(paste("Planting in", plantMonth, "at", yield_level,  sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
        print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        #print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
        dev.off()
        
      })  
      
      #.............................................................................................................
  
      
      #.............................................................................................................
      #download buttons for printablse guides based on unit of land and use case selection
      if (usecase == "Fertilizer Recommendation" & unit == "acre"){
        
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (acre)", "pdf", sep=".")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
        
        # #download acre printable guides
        # output$downloadDatafr <- downloadHandler(
        #   filename <- function() {
        #     paste("FR Printable guides (acre)",  ".pdf", sep="")
        #   },
        #   
        #   content <- function(file) {
        #     file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Acre latest.pdf", file)
        #   },
        #   contentType = "application/pdf"
        # )
        
      }else if(usecase == "Fertilizer Recommendation" & unit == "ha"){
        
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("FR Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Tailored fertilizer application recommendations for cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        ) 
        
        
      }else if (usecase == "Scheduled Planting" & unit == "acre"){
        #download acre printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (acre)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Acre latest.pdf", file)
          },
          contentType = "application/pdf"
        )
      }else if(usecase == "Scheduled Planting" & unit == "ha"){
        
        #download hectare printable guides
        output$downloadDatafr <- downloadHandler(
          filename <- function() {
            paste("SP Printable guides (ha)",  ".pdf", sep="")
          },
          
          content <- function(file) {
            file.copy("data/Scheduled Planting and Harvest Cassava - Nigeria Hectare latest.pdf", file)
          },
          contentType = "application/pdf"
        )  
        
      } 
      
      
     
      #TANZANIA COMPONENT 
    }else if(country == 'Tanzania'){
    
      #define the yield category
      if (unit == "ha"){
        FCY <- FCY_ha
        
        if(FCY == "7.5-15 t/ha" ){
          ds=FR_TZ_FCY2_plm
        } else if(FCY == "0-7.5 t/ha" ){
          ds=FR_TZ_FCY1_plm
        }else if(FCY == "15-22.5 t/ha" ){
          ds=FR_TZ_FCY3_plm
        }else if(FCY == "22.5-30 t/ha" ){
          ds <- FR_TZ_FCY4_plm
        }else if(FCY == ">30 t/ha" ){
          ds <- FR_TZ_FCY5_plm
        }
      }else if(unit == "acre"){
        FCY <- FCY_acre
        if(FCY == "3-6 t/acre" ){
          ds=FR_TZ_FCY2_plm
        } else if(FCY == "0-3 t/acre"  ){
          ds=FR_TZ_FCY1_plm
        }else if(FCY == "6-9 t/acre" ){
          ds=FR_TZ_FCY3_plm
        }else if(FCY == "9-12 t/acre" ){
          ds <- FR_TZ_FCY4_plm
        }else if(FCY == ">30 t/acre" ){
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
      
      if(unit == "ha"){
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
      
      
      colnames(tt_tz2) <- c("Region","DISTRICT", "Urea (kg/ha)", "NPK 17:17:17 (kg/ha)", "DAP kg/ha", "Expected yield response (t))"
                                  )
    
      #table output based on cost inputs
      
      if(costs == "No"){
       #  output$mytable = DT::renderDataTable({
       #    data_output(tt_tz2)
       #  }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
       # )
        
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
        
        df_tt2 <- data.frame(UreaPrice=input$UreaPrice,NPK171717Price=input$NPK171717Price,CassavaPrice=input$CassavaPrice,DAPPrice=input$DAPPrice,
                            REGION=input$lga_Groups)
        
        return(df_tt2)
      })
      
            #df_tt <- data.frame(UreaPrice=UreaPrice,NPK171717Price=NPK171717Price,CassavaPrice=CassavaPrice,REGION=lga_Groups,DAPPrice=DAPPrice)
      #  
      print(CassavaPrice)
      tt_merge_tz <- merge(tt_tz, tt_dataframe2(),by="REGION")
      
      tt_merge_tz$totalSalePrice = as.numeric(tt_merge_tz$LGAdY)  * as.numeric(tt_merge_tz$CassavaPrice)
      tt_merge_tz$totalCost = (as.numeric(tt_merge_tz$UreaPrice)/50 * as.numeric(tt_merge_tz$Urea)) + 
        (as.numeric(tt_merge_tz$NPK171717Price)/50 * as.numeric(tt_merge_tz$NPK17_17_17))+
        (as.numeric(tt_merge_tz$DAPPrice)/50 * as.numeric(tt_merge_tz$DAP))
      #tt_merge$NetRevenue = tt_merge$totalSalePrice - tt_merge$totalCost
      #totalCost = (as.numeric(UreaPrice)/50 * 15) + (as.numeric(NPK151515Price)/50 * 300)
      
      tt_merge_tz$NetRevenue = as.numeric(tt_merge_tz$totalSalePrice) - as.numeric(tt_merge_tz$totalCost)
      
      tt_merge_tz2 <- dplyr::select(tt_merge_tz, c(REGION, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
      colnames(tt_merge_tz2) <- c("Region","District", "Urea (kg/ha)", "NPK 17:17:17 (kg/ha)", "DAP", "Expected yield increase (t)", 
                               "Cassava Price", "Total sale (TZS)", "Fertilizer cost (TZS)", "Profit (TZS)")
      
      
      write.csv(tt_merge_tz2, fileNameCsv, row.names = FALSE)
      
      AOIMap3 <- st_as_sf(AOIMap2)
      
     #  output$mytable2 = DT::renderDataTable({
     #    data_output(tt_merge_tz2)
     #  }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
     # )
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
        
        if(unit == "ha"){
          
          tturea <- paste("Recommended urea rate(kg/ha)")
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
        
        if(unit == "ha"){
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
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
        
        if(unit == "ha"){
          
          ttdap <- paste("Recommended DAP (kg/ha)")
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
        
        if(unit == "ha"){
          
          ttha <- paste("Recommended Yield response (t/ha)")
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
      if(unit == "ha"){
        ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                      "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
        ttz <- "Urea (kg/ha)"
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
      if(unit == "ha"){
        NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                     "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
        ttz <- "NPK 17-17-17 (kg/ha)"
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
      if(unit == "ha"){
        DAPcols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                     "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
        ttz <- "DAP (kg/ha)"
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
      
      if(unit == "ha"){
        ttz <- "Yield increase (t/ha)"
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
        }else if (Selection2 == "NPK 15:15:15 rate"){
          filt_select <- "NPK 15:15:15 rate"
        }else{
          filt_select <- "DAP rate"
          
        }
        
      })
      
      # choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
      # choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),
      
      #show map based on selection of variable but retaining single name
      
      #filter by variable selected and unit for color pallette
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          #reactive legend title
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Recommended urea rate(kg/ha)")
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
            
            if(unit == "ha"){
              
              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
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
            
            if(unit == "ha"){
              
              ttdap <- paste("Recommended DAP rate (kg/ha)")
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
            
            if(unit == "ha"){
              
              ttha <- paste("Recommended yield increase (t/ha)")
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
      
  
   
      if ( usecase == "Fertilizer Recommendation" & unit == "acre"){
        
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
        
      }else if(usecase == "Fertilizer Recommendation" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      }else if (!is.null(input$btn_go) & usecase == "Scheduled Planting" & unit == "acre"){
        #download acre printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(!is.null(input$btn_go)  & usecase == "Scheduled Planting" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          )  
        
        
      }  
      
      
     
      
      
    }else if(country == 'Rwanda'){
      
      #define the yield category
      if (unit_rw == "ha"){
        FCY <- FCY_ha_rw
        
        if(FCY == "7.5-15 t/ha" ){
          ds=FR_RW_FCY2_plm
        } else if(FCY == "0-7.5 t/ha" ){
          ds=FR_RW_FCY1_plm
        }else if(FCY == "15-22.5 t/ha" ){
          ds=FR_RW_FCY3_plm
        }else if(FCY == "22.5-30 t/ha" ){
          ds <- FR_RW_FCY4_plm
        }else if(FCY == ">30 t/ha" ){
          ds <- FR_RW_FCY5_plm
        }
      }else if(unit_rw == "are"){
        FCY <- FCY_are_rw
        if(FCY == "3-6 t/are" ){
          ds=FR_RW_FCY2_plm
        } else if(FCY == "0-3 t/are"  ){
          ds=FR_RW_FCY1_plm
        }else if(FCY == "6-9 t/are" ){
          ds=FR_RW_FCY3_plm
        }else if(FCY == "9-12 t/are" ){
          ds <- FR_RW_FCY4_plm
        }else if(FCY == ">30 t/are" ){
          ds <- FR_RW_FCY5_plm
        }
      }
      
     colnames(ds) <- c("plDate","N","P", "K","WLY","CurrentY","TargetY","TC","NR","NPK17_17_17","DAP","MOP",
                       "Urea","FCY","plw","harvMonth","STATE","DISTRICT","respY","groRev", "plm","rateNPK171717", "rateDAP")
     #subset dataset by STATEs
     
     Iburasirazuba <- droplevels(ds[ds$STATE %in% c("Iburasirazuba"), ])
     Iburasirazubalabel <- data.frame(STATE= c("Iburasirazuba"), lon=c(30.44), lat=c(-1.78))
      
     Amajyepfo    <- droplevels(ds[ds$STATE %in% c("Amajyepfo"), ])
     Amajyepfolabel <- data.frame(STATE= c("Amajyepfo"), lon=c(29.6), lat=c(-2.55))
      
     Amajyaruguru <- droplevels(ds[ds$STATE %in% c("Amajyaruguru"), ])
     Amajyarugurulabel <- data.frame(STATE= c("Amajyaruguru"), lon=c(29.88), lat=c(-1.66))
      
     Iburengerazuba <- droplevels(ds[ds$STATE %in% c("Iburengerazuba"), ])
     Iburengerazubalabel <- data.frame(STATE= c("Iburengerazuba"), lon=c(29.33), lat=c(-2.08))
    
     UmujyiwaKigali <- droplevels(ds[ds$STATE %in% c("Umujyi wa Kigali"), ])
     UmujyiwaKigalilabel <- data.frame(STATE= c("Umujyi wa Kigali"), lon=c(30.11), lat=c(-1.96))
      
   
      
      if(lgaGroups =="Amajyaruguru"){
        LGApoints <- Amajyaruguru     
        stateLabel <- Amajyarugurulabel
        textangle<-0
        cities = c("Byumba")
        couple <- "One"
        
      }else if(lgaGroups =="Amajyepfo"){
        LGApoints <- Amajyepfo
        stateLabel <- Amajyepfolabel
        textangle<-0
        cities = c("Nyanza")
        couple <- "One"
        
      }else if(lgaGroups =="Iburengerazuba"){
        LGApoints <- Iburengerazuba     
        stateLabel <- Iburengerazubalabel
        textangle<-0 
        cities = c("Kibuye")
        couple <- "One"
        
      }else if(lgaGroups =="Iburasirazuba"){
        LGApoints <- Iburasirazuba 
        stateLabel <- Iburasirazubalabel
        textangle<-0 
        cities = c("Rwamagana")
        couple <- "One"
        
      }else if(lgaGroups =="Umujyi wa Kigali"){
        LGApoints <- UmujyiwaKigali 
        stateLabel <- UmujyiwaKigalilabel
        textangle<-0 
        cities = c("Kigali City")
        couple <- "One"
        
      }
    
     plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
     
     if(couple == "Two"){
       lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
     }
     
     if(couple == "Three"){
       lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
     }
     
     plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
     
     AOI <- lgaGroups
     AOIMapS <- subset(boundaryRW, NAME_1 %in% AOI ) 
     
     AOIMap <- subset(rwState, NAME_1 %in% AOI )
     AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
     LGAnames <- as.data.frame(AOIMap)
     LGAnames <- cbind(LGAnames, coordinates(AOIMap))
     colnames(LGAnames) <- c("STATE","DISTRICT","long","lat"  )
     crop_ngstate <- subset(rwState, NAME_1 %in% AOI )
     
     
     ## take REGION average
     LGAaverage <- ddply(plotData, .(DISTRICT, STATE), summarize,
                         LGAUrea = round(mean(Urea), digits=0),
                         LGANPK171717 = round(mean(rateNPK171717), digits=0),
                         LGADAP = round(mean(rateDAP), digits=0),
                         LGAdY = round(mean(respY), digits=0))
     
     
     dss <- LGAaverage
     dss$LGAUrea <- dss$LGAUrea / 2.47105
     dss$LGANPK171717 <- dss$LGANPK171717 / 2.47105
     dss$LGADAP <- dss$LGADAP / 2.47105
     dss$LGAdY <- dss$LGAdY / 2.47105
     
     if(unit_rw == 'acre'){
       LGAaverage <- dss
     }
     
     plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "STATE"))
     
     if(unit_rw == "ha"){
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
     
     AOIMap2 <- merge(AOIMap, unique(plotData[, c("STATE","DISTRICT", "Urea", "NPK17_17_17","DAP","dY", "LGAdY")]),
                      by.x=c("NAME_1","NAME_2") ,by.y=c("STATE","DISTRICT"))
     AOIMap2$month <- plantMonth
     AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
     plotData$month <- plantMonth
     
     Currency <- "RWF"
     tt_rw <- unique(as.data.frame(plotData[, c("STATE","DISTRICT", "Urea", "NPK17_17_17","DAP", "LGAdY", "month")]))
     tt_rw$LGAdY <- round(tt_rw$LGAdY, digits = 1)
     tt_rw2 <- dplyr::select(tt_rw, c(STATE, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY))
     
     
     colnames(tt_rw2) <- c("State","District", "Recommended urea rate", "NPK17_17_17 rate", "DAP rate", "Expected yield response"
     )
     
     #table output based on cost inputs
     
     if(costs == "No"){
      #  output$mytable = DT::renderDataTable({
      #    data_output(tt_rw2)
      #  }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
      # )
       
       output$tabletext_rwf<- renderText({
         
         
         paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
         
       })
       
       output$mytable <- renderDT({tt_rw2},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_rw2),
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
         
         df_rw2 <- data.frame(UreaPrice=input$UreaPrice,NPK171717Price=input$NPK171717Price,CassavaPrice=input$CassavaPrice,DAPPrice=input$DAPPrice,
                              STATE=input$state)
         
         return(df_rw2)
       })
       
       #df_tt <- data.frame(UreaPrice=UreaPrice,NPK171717Price=NPK171717Price,CassavaPrice=CassavaPrice,REGION=lga_Groups,DAPPrice=DAPPrice)
       #  
       print(CassavaPrice)
       tt_merge_rw <- merge(tt_rw, tt_dataframe2(),by="STATE")
       
       tt_merge_rw$totalSalePrice = as.numeric(tt_merge_rw$LGAdY)  * as.numeric(tt_merge_rw$CassavaPrice)
       tt_merge_rw$totalCost = (as.numeric(tt_merge_rw$UreaPrice)/50 * as.numeric(tt_merge_rw$Urea)) + 
         (as.numeric(tt_merge_rw$NPK171717Price)/50 * as.numeric(tt_merge_rw$NPK17_17_17))+
         (as.numeric(tt_merge_rw$DAPPrice)/50 * as.numeric(tt_merge_rw$DAP))
       #tt_merge$NetRevenue = tt_merge$totalSalePrice - tt_merge$totalCost
       #totalCost = (as.numeric(UreaPrice)/50 * 15) + (as.numeric(NPK151515Price)/50 * 300)
       
       tt_merge_rw$NetRevenue = as.numeric(tt_merge_rw$totalSalePrice) - as.numeric(tt_merge_rw$totalCost)
       
       tt_merge_rw2 <- dplyr::select(tt_merge_rw, c(STATE, DISTRICT, Urea, NPK17_17_17, DAP, LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
       colnames(tt_merge_rw2) <- c("State","District", "Urea (kg/ha)", "NPK 17:17:17 (kg/ha)", "DAP (kg/ha)","Expected yield increase (t)",
                                   "Cassava Price",  "Total sale (RWF)", "Fertilizer cost (RWF)", "Profit (RWF)")
       
       
       write.csv(tt_merge_rw2, fileNameCsv, row.names = FALSE)
       
       AOIMap3 <- st_as_sf(AOIMap2)
       
       output$tabletext_rwf<- renderText({
         
         
         paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
         
       })
       
       output$mytable2 <- renderDT({tt_merge_rw2},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_merge_rw2),
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
        
        if(unit_rw == "ha"){
          
          tturea <- paste("Recommended urea rate(kg/ha)")
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
        
        if(unit_rw == "ha"){
          
          ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
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
        
        if(unit_rw == "ha"){
          
          ttdap <- paste("Recommended DAP (kg/ha)")
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
        
        if(unit_rw == "ha"){
          
          ttha <- paste("Recommended Yield response (t/ha)")
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
      if(unit_rw == "ha"){
        ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                      "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
        trw <- "Urea (kg/ha)"
      }else {
        ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                      "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
        trw <- "Urea (kg/are)"
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
        geom_text(data=stateLabel, aes(lon, lat, label=STATE, fontface=2), col='black', size=6)+
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_fancy_orienteering) +
        # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
        #                        style = north_arrow_fancy_orienteering) +
        # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
        xlab("") + ylab("") +
        ggtitle(trw) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))  

      # 
      # 
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      if(unit_rw == "ha"){
        NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929",
                     "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
        trw <- "NPK 17-17-17 (kg/ha)"
      }else{
        NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929",
                     "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
        trw <- "NPK 17-17-17 (kg/are)"
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
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(trw) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))


      # DAP color pallette
      DAPPpalette <- brewer.pal(9,"YlGnBu")
      if(unit_rw == "ha"){
        DAPcols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                     "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
        trw <- "DAP (kg/ha)"
      }else{
        DAPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                     "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
        trw <- "DAP (kg/are)"
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
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(trw) +
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

      if(unit_rw == "ha"){
        trw <- "Yield increase (t/ha)"
      }else{
        trw <- "Yield increase (t/are)"
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
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(trw) +
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

      # #-------------------------------------------------------------------------
      # #front page dynamic tmap
      # #-------------------------------------------------------------------------
      # 
      #reactive selection of variable to view
      filt_select <- reactive({
        print(Selection2)
        if (Selection2 == "Urea rate"){
          filt_select <- "Urea rate"
        }else if (Selection2 == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else if (Selection2 == "NPK 17:17:17 rate"){
          filt_select <- "NPK 17:17:17 rate"
        }else if (Selection2 == "NPK 15:15:15 rate"){
          filt_select <- "NPK 15:15:15 rate"
        }else{
          filt_select <- "DAP rate"

        }

      })

      # choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
      # choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),

      #show map based on selection of variable but retaining single name

      #filter by variable selected and unit for color pallette
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){

          ureacols <- reactive({

            if(unit_rw == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/are)"
            }
          })

          #reactive legend title
          tturea <- reactive({

            if(unit_rw == "ha"){

              tturea <- paste("Recommended urea rate(kg/ha)")
            }else {

              tturea <- paste("Recommended urea rate (kg/are)")
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

            if(unit_rw == "ha"){

              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/ha)")
            }else {

              ttnpk <- paste("Recommended NPK 17:17:17 rate (kg/are)")
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

            if(unit_rw == "ha"){

              ttdap <- paste("Recommended DAP rate (kg/ha)")
            }else {

              ttdap <- paste("Recommended DAP rate (kg/are)")
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

            if(unit_rw == "ha"){

              ttha <- paste("Recommended yield increase (t/ha)")
            }else {

              ttha <- paste("Recommended yield increase (t/are)")

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
      
    
      
      
      if ( usecase == "Fertilizer Recommendation" & unit_rw == "are"){
        
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
        
      }else if(usecase == "Fertilizer Recommendation" & unit_rw == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      }else if (!is.null(input$btn_go) & usecase == "Scheduled Planting" & unit_rw == "are"){
        #download acre printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(!is.null(input$btn_go)  & usecase == "Scheduled Planting" & unit_rw == "ha"){
        #download hectare printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          )  
        
        
      }  
 
      #GHANA STARTS HERE
    }else if (country=="Ghana"){
      
      #define the yield category
      if (unit == "ha"){
        #FCY_ha = "0-7.5 t/ha"
        FCY <- FCY_ha
        
        if(FCY == "7.5-15 t/ha" ){
          ds=FR_GH_FCY2_plm
        } else if(FCY == "0-7.5 t/ha" ){
          ds=FR_GH_FCY1_plm
        }else if(FCY == "15-22.5 t/ha" ){
          ds=FR_GH_FCY3_plm
        }else if(FCY == "22.5-30 t/ha" ){
          ds <- FR_GH_FCY4_plm
        }else if(FCY == ">30 t/ha" ){
          ds <- FR_GH_FCY5_plm
        }
      }else if(unit == "acre"){
        FCY <- FCY_acre
        if(FCY == "3-6 t/acre" ){
          ds=FR_GH_FCY2_plm
        } else if(FCY == "0-3 t/acre"  ){
          ds=FR_GH_FCY1_plm
        }else if(FCY == "6-9 t/acre" ){
          ds=FR_GH_FCY3_plm
        }else if(FCY == "9-12 t/acre" ){
          ds <- FR_GH_FCY4_plm
        }else if(FCY == ">30 t/acre" ){
          ds <- FR_GH_FCY5_plm
        }
      }
      
      # colnames(ds) <- c("plDate","N","P", "K","WLY","CurrentY","TargetY","TC","NR","NPK17_17_17","DAP","MOP",
      #                    "Urea","FCY","plw","harvMonth","STATE","DISTRICT","respY","groRev", "plm","rateNPK171717", "rateDAP")
      # #subset dataset by STATEs
   
      
      Savannah <- droplevels(ds[ds$Regions %in% c("Savannah"), ])
      Savannahlabel <-  data.frame(Regions= c("Savannah"), lon=c(-0.75), lat=c(9.5))
      
      Bono_East <- droplevels(ds[ds$Regions %in% c("Bono East"), ])
      Bono_Eastlabel <- data.frame(Regions= c("Bono East"), lon=c(-0.65), lat=c(8.5))
      
      Bono <- droplevels(ds[ds$Regions %in% c("Bono"), ])
      Bonolabel <- data.frame(Regions= c("Brong Ahafo"), lon=c(-2.5), lat=c(8.68))
      
      
      Central <- droplevels(ds[ds$Regions %in% c("Central"), ])
      Centrallabel <- data.frame(Regions= c("Central"), lon=c(-1.0), lat=c( 6.1))
      
      Ashanti <- droplevels(ds[ds$Regions %in% c("Ashanti"), ])
      Ashantilabel <- data.frame(Regions= c("Ashanti"), lon=c(-2.5), lat=c(7.53))
      
      Ahafo <- droplevels(ds[ds$Regions %in% c("Ahafo"), ])
      Ahafolabel <- data.frame(Regions= c("Ahafo"), lon=c(-2.8), lat=c(6.5))
      
      
      Eastern <- droplevels(ds[ds$Regions %in% c("Eastern"), ])
      Eastlabel <- data.frame(Regions= c("Eastern"), lon=c(-0.8), lat=c(7.21))
      
      Volta <- droplevels(ds[ds$Regions %in% c("Volta"), ])
      Voltalabel <- data.frame(Regions= c("Volta"), lon=c(0.7), lat=c(7.3))
      
      
      Savannahcity <- data.frame(Regions = c("Savannah"),name=c("Damongo"), lat=c(9.08), lon = c(-1.82))
      
      Centralcity <- data.frame(Regions = c("Central"),name=c("Cape Coast"), lat=c(5.12), lon = c(-1.27))
      
      Bonocity <- data.frame(Regions = c("Brong Ahafo"), name=c("Sunyani"), 
                             lat=c(7.34), lon = c(-2.32))
      
      Bono_Eastcity <- data.frame(Regions = c("Bono East"), name=c("Techiman"), 
                                  lat=c(7.58), lon = c(-1.93))
      
      Ashanti_city <- data.frame(Regions = c("Ashanti"), name=c("Kumasi"), 
                                 lat=c(6.68), lon = c(-1.63))
      
      Ahafocity <- data.frame(Regions = c("Ahafo"), name=c("Goaso"), 
                              lat=c(6.8), lon = c(-2.52))
      
      
      Eastcity <- data.frame(Regions = c("Eastern"),name=c("Koforidua"), 
                             lat=c(6.1), lon = c(-0.26))
      
      
      Voltacity <- data.frame(Regions = c("Volta"),name=c("Ho"), 
                              lat=c(6.6), lon = c(0.47))
      
     if(lgaGroups =="Ashanti"){
        LGApoints <- Ashanti
        RegionsLabel <- Ashantilabel
        textangle<-0
        cities = c("Ashanti_city")
        couple <- "Two"
        
      }else if(lgaGroups =="Bono"){
        LGApoints <- Bono
        RegionsLabel <- Bonolabel
        textangle<-0
        cities = c("Bonocity")
        couple <- "Two"
        
      }else if(lgaGroups =="Bono East"){
        LGApoints <- Bono_East
        RegionsLabel <- Bono_Eastlabel
        textangle<-0
        cities = c("Bono_Eastcity")
        couple <- "Two"
        
      }else if(lgaGroups =="Savannah"){
        LGApoints <- Savannah
        RegionsLabel <- Savannahlabel
        textangle<-0
        cities = c("Savannahcity")
        couple <- "Two"
        
      }else if(lgaGroups =="Ahafo"){
        LGApoints <- Ahafo     
        RegionsLabel <- Ahafolabel
        textangle<-0 
        cities = c("Ahafocity")
        couple <- "Two"
        
      }else if(lgaGroups =="Central"){
        LGApoints <- Central 
        RegionsLabel <- Centrallabel
        textangle<-0 
        cities = c("Centralcity")
        couple <- "Two"
        
      }else if(lgaGroups =="Volta"){
        LGApoints <- Volta 
        RegionsLabel <- Voltalabel
        textangle<-0 
        cities = c("Voltacity")
        couple <- "Two"
        
      }else if(lgaGroups =="Eastern"){
        LGApoints <- Eastern 
        RegionsLabel <- Eastlabel
        textangle<-0 
        cities = c("Eastcity")
        couple <- "Two"
        
      }
      
      plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$Regions %in% lgaGroups , ])
      
      AOI <- lgaGroups
      AOIMapS <- subset(boundaryGH, ADM1_EN %in% AOI ) 
      
      AOIMap <- subset(ghRegions, ADM1_EN %in% AOI )
      AOIMap <- AOIMap[,c("ADM1_EN", "ADM2_EN")]
      LGAnames <- as.data.frame(AOIMap)
      LGAnames <- cbind(LGAnames, coordinates(AOIMap))
      colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
      crop_ghRegions <- subset(ghRegions, ADM1_EN %in% AOI )
      
      
      ## take REGION average
      LGAaverage <- ddply(plotData, .(Districts, Regions), summarize,
                          LGAUrea = round(mean(Urea), digits=0),
                          LGANPK112221 = round(mean(NPK112221 ), digits=0),
                          LGANPK251010= round(mean(NPK251010), digits=0),
                          LGANPK152020 = round(mean(NPK152020), digits=0),
                          LGANPK123017 = round(mean(NPK123017), digits=0),
                          LGAdY = round(mean(respY), digits=0))
      
      
      LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
      LGAaverage$LGANPK112221 <- ifelse(LGAaverage$LGANPK112221 <25, 0, LGAaverage$LGANPK112221)
      LGAaverage$LGANPK251010 <- ifelse(LGAaverage$LGANPK251010 <25, 0, LGAaverage$LGANPK251010)
      LGAaverage$LGANPK152020 <- ifelse(LGAaverage$LGANPK152020 <25, 0, LGAaverage$LGANPK152020)
      LGAaverage$LGANPK123017 <- ifelse(LGAaverage$LGANPK123017 <25, 0, LGAaverage$LGANPK123017)
      
      
      
      if(unit == 'acre'){
        
        dss <- LGAaverage
        dss$LGAUrea <- dss$LGAUrea / 2.47105
        dss$LGANPK112221 <- dss$LGANPK112221 / 2.47105
        dss$LGANPK251010 <- dss$LGANPK251010 / 2.47105
        dss$LGANPK152020 <- dss$LGANPK152020 / 2.47105
        dss$LGANPK123017 <- dss$LGANPK123017 / 2.47105
        dss$LGAdY <- dss$LGAdY / 2.47105
        
        LGAaverage <- dss
      }
      
      plotData <- merge(plotData, LGAaverage, by=c("Districts", "Regions"))
      
      if(unit == "ha"){
        plotData$Urea <- round(plotData$LGAUrea/50)*50
        plotData$NPK112221 <- round(plotData$LGANPK112221/50)*50
        plotData$NPK251010 <- round(plotData$LGANPK251010 /25)*25
        plotData$NPK152020 <- round(plotData$LGANPK152020/50)*50
        plotData$NPK123017 <- round(plotData$LGANPK123017 /25)*25
        plotData$dY <- round(plotData$LGAdY/2)*2
      }else{
        plotData$Urea <- round(plotData$LGAUrea/10)*10
        plotData$NPK112221 <- round(plotData$LGANPK112221/10)*10
        plotData$NPK251010 <- round(plotData$LGANPK251010 /10)*10
        plotData$NPK152020 <- round(plotData$LGANPK152020/10)*10
        plotData$NPK123017 <- round(plotData$LGANPK123017 /10)*10
        plotData$dY <- round(plotData$LGAdY/1)*1
      }
      
      fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
      
      AOIMap2 <- merge(AOIMap, unique(plotData[, c("Regions","Districts", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017","dY", "LGAdY")]),
                       by.x=c("ADM1_EN","ADM2_EN") ,by.y=c("Regions","Districts"))
      AOIMap2$month <- plantMonth
      AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
      plotData$month <- plantMonth
    
      Currency <- "GHC"
      tt_gh <- unique(as.data.frame(plotData[, c("Regions","Districts", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017", "LGAdY", "month")]))
      tt_gh$LGAdY <- round(tt_gh$LGAdY, digits = 1)
      tt_gh2 <- dplyr::select(tt_gh, c("Regions","Districts", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017", "LGAdY"))
      
      
      colnames(tt_gh2) <- c("Regions","Districts", "Recommended urea rate", "NPK11_22_21 rate", "NPK25_10_10 rate", "NPK15_20_20 rate","NPK12_30_17 rate",
                             "Expected yield response"
      )
      
      #table output based on cost inputs
      
      if(costs == "No"){
        #  output$mytable = DT::renderDataTable({
        #    data_output(tt_rw2)
        #  }, caption=paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
        # )
        
        output$tabletext_ghc<- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
          
        })
        
        output$mytable <- renderDT({tt_gh2},
                                   rownames = FALSE, 
                                   extensions = c('Buttons','FixedColumns'), 
                                   options = list(dom = 'Bfrtip',
                                                  pageLength = nrow(tt_gh2),
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
        #colnames(tt) <- c("Regions","LGA", "Recommended urea rate (kg/ha)", "NPK15_15_15 rate", "Expected yield response", "Planting month")
        tt_dataframe2 <- reactive({
          
          df_gh2 <- data.frame(UreaPrice=input$UreaPrice,NPK112221Price=input$NPK112221Price, NPK251010Price=input$NPK251010Price,
                               NPK1520207Price=input$NPK152020Price,NPK123017Price=input$NPK123017Price,CassavaPrice=input$CassavaPrice,
                               Regions=input$reg_ghana)
          
          return(df_gh2)
        })
        

        #df_tt <- data.frame(UreaPrice=UreaPrice,NPK171717Price=NPK171717Price,CassavaPrice=CassavaPrice,REGION=lga_Groups,DAPPrice=DAPPrice)
        #  
        print(CassavaPrice)
        tt_merge_gh <- merge(tt_gh, tt_dataframe2(),by="Regions")
        
        
        #MEKLIT HERE
        tt_merge_gh$totalSalePrice = as.numeric(tt_merge_gh$LGAdY)  * as.numeric(tt_merge_gh$CassavaPrice)
        tt_merge_gh$totalCost = (as.numeric(tt_merge_gh$UreaPrice)/50 * as.numeric(tt_merge_gh$Urea)) + 
          (as.numeric(tt_merge_gh$NPK112221Price)/50 * as.numeric(tt_merge_gh$NPK112221))+
          (as.numeric(tt_merge_gh$NPK251010Price)/50 * as.numeric(tt_merge_gh$NPK251010))+
          (as.numeric(tt_merge_gh$NPK1520207Price)/50 * as.numeric(tt_merge_gh$NPK152020))+
          (as.numeric(tt_merge_gh$NPK123017Price)/50 * as.numeric(tt_merge_gh$NPK123017))
         
        #tt_merge$NetRevenue = tt_merge$totalSalePrice - tt_merge$totalCost
        #totalCost = (as.numeric(UreaPrice)/50 * 15) + (as.numeric(NPK151515Price)/50 * 300)
        
        tt_merge_gh$NetRevenue = as.numeric(tt_merge_gh$totalSalePrice) - as.numeric(tt_merge_gh$totalCost)
        
        tt_merge_gh2 <- dplyr::select(tt_merge_gh, c(Regions, Districts, Urea, NPK112221,NPK251010,NPK152020,NPK123017,  LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
        colnames(tt_merge_gh2) <- c("Regions","Districts", "Urea (kg/ha)", "NPK 11:22:21 (kg/ha)", "NPK 15:20:20 (kg/ha)","NPK 12:30:17 (kg/ha)","NPK 25:10:10 (kg/ha)",
                                    "Expected yield increase (t)",  "Cassava Price",  "Total sale (GHC)", "Fertilizer cost (GHC)", "Profit (GHC)")
        
        
        write.csv(tt_merge_gh2, fileNameCsv, row.names = FALSE)
        
        AOIMap3 <- st_as_sf(AOIMap2)
        
        output$tabletext_ghc<- renderText({
          
          
          paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
          
        })
        
        
        output$mytable2 <- renderDT({tt_merge_gh2},
                                    rownames = FALSE, 
                                    extensions = c('Buttons','FixedColumns'), 
                                    options = list(dom = 'Bfrtip',
                                                   pageLength = nrow(tt_merge_gh2),
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
        
        if(unit == "ha"){
          
          tturea <- paste("Recommended urea rate(kg/ha)")
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
                         tm_text(text = "ADM2_EN")
                       sm1
                       
                       
                       
                     })
                   })
      ############################################################################   
      #npk 11:22:21 plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk11 <- reactive({
        
        if(unit == "ha"){
          
          ttnpk11 <- paste("Recommended NPK 11:22:21 rate (kg/ha)")
        }else {
          
          ttnpk11 <- paste("Recommended NPK 11:22:21 rate(kg/acre)")
        }
      })
      
      
      
      npk11 <- unique(AOIMap3$NPK112221)
      kev <- as.character(npk11[order(npk11)])
      AOIMap3$NPK112221 <- factor(AOIMap3$NPK112221)
      levels(AOIMap3$NPK112221) <- kev
      
      #npk plot
      observeEvent(ttnpk11(),
                   {
                     
                     output$npkplot_11 <- renderTmap({
                       
                       
                       
                       nm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK112221",
                           title = ttnpk11(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Purples")+
                         tm_text(text = "ADM2_EN")
                       
                      nm2
                       
                     }) 
                   })
      
      ############################################################################   
      #NPK25:10:10 plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk25 <- reactive({
        
        if(unit == "ha"){
          
          ttnpk25<- paste("Recommended NPK 25:10:10 rate (kg/ha)")
        }else {
          
          ttnpk25 <- paste("Recommended NPK 25:10:10 rate(kg/acre)")
        }
      })
      
      
      
      npk25 <- unique(AOIMap3$NPK251010)
      kev <- as.character(npk25[order(npk25)])
      AOIMap3$NPK251010 <- factor(AOIMap3$NPK251010)
      levels(AOIMap3$NPK251010) <- kev
      
      #npk plot
      observeEvent(ttnpk25(),
                   {
                     
                     output$npkplot_25 <- renderTmap({
                       
                       
                       
                       nm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK251010",
                           title = ttnpk25(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Reds")+
                         tm_text(text = "ADM2_EN")
                       
                       nm3
                       
                     }) 
                   })
      ############################################################################   
      #npk NPK12:30:17 plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk12 <- reactive({
        
        if(unit == "ha"){
          
          ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/ha)")
        }else {
          
          ttnpk12 <- paste("Recommended NPK 12:30:17 rate(kg/acre)")
        }
      })
      
      
      
      npk12 <- unique(AOIMap3$NPK123017)
      kev <- as.character(npk12[order(npk12)])
      AOIMap3$NPK123017 <- factor(AOIMap3$NPK123017)
      levels(AOIMap3$NPK123017) <- kev
      
      #npk plot
      observeEvent(ttnpk12(),
                   {
                     
                     output$npkplot_12 <- renderTmap({
                       
                       
                       
                       nm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK123017",
                           title = ttnpk12(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "ADM2_EN")
                       
                       nm4
                       
                     }) 
                   })
      ############################################################################   
      #npk NPK 15:20:20 plot
      #############################################################################
      #reactive title based on unit of land
      
      ttnpk15 <- reactive({
        
        if(unit == "ha"){
          
          ttnpk15 <- paste("Recommended NPK 15:20:20 rate (kg/ha)")
        }else {
          
          ttnpk15 <- paste("Recommended NPK 15:20:20 rate(kg/acre)")
        }
      })
      
      
      
      npk15 <- unique(AOIMap3$NPK152020)
      kev <- as.character(npk15[order(npk15)])
      AOIMap3$NPK152020 <- factor(AOIMap3$NPK152020)
      levels(AOIMap3$NPK152020) <- kev
      
      #npk plot
      observeEvent(ttnpk15(),
                   {
                     
                     output$npkplot_15 <- renderTmap({
                       
                       
                       
                       nm1 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "NPK152020",
                           title = ttnpk15(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Purples")+
                         tm_text(text = "ADM2_EN")
                       
                       nm1
                       
                     }) 
                   })
      

      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
      ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Recommended Yield response (t/ha)")
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
                         tm_text(text = "ADM2_EN")
                       
                       sm3
                       
                     })
                   })
 
      #-------------------------------
      #generate downloadable maps
      #-------------------------------
      print(plantMonth)
      #generate color pallette
      if(unit == "ha"){
        ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                      "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
        tgh <- "Urea (kg/ha)"
      }else {
        ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                      "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
        tgh <- "Urea (kg/acre)"
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
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        geom_text(data=RegionsLabel, aes(lon, lat, label=Regions, fontface=2), col='black', size=6)+
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
        annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                               style = north_arrow_fancy_orienteering) +
        # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
        #                        style = north_arrow_fancy_orienteering) +
        # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
        xlab("") + ylab("") +
        ggtitle(tgh) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))  
      
      # 
      # 
      ### NPK NPK11:22:21  palette <- brewer.pal(9,"YlOrBr")
      if(unit == "ha"){
        NPKcols11 <- c("0"="#FFFFFF","25"= "#D9F0A3", "50"= "#ADDD8E", "75"= "#78C679",
                       "100"= "#41AB5D", "125"= "#238443", "150"= "#006837", "175"= "#004529")
        tgh11 <- "NPK 11:22:21 (kg/ha)"
      }else{
        NPKcols11 <- c("0"="#FFFFFF","10"= "#D9F0A3", "20"= "#ADDD8E", "30"= "#78C679",
                       "40"= "#41AB5D", "50"= "#238443", "60"= "#006837", "70"= "#004529")
        tgh11 <- "NPK 11:22:21 (kg/acre)"
      }
      
      
      npk11 <- unique(AOIMap3$NPK112221)
      kev <- as.character(npk11[order(npk11)])
      AOIMap3$NPK112221  <- factor(AOIMap3$NPK112221)
      levels(AOIMap3$NPK112221 ) <- kev
      
      #ggplot NPK
      ggNPK11 <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK112221), col="darkgrey") +
        scale_fill_manual(values = NPKcols11, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
       # geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tgh11) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      ### NPK25:10:10 palette <- brewer.pal(9,"YlOrBr")
      if(unit == "ha"){
        NPKcols25 <- c("0"="#FFFFFF","25"= "#BFD3E6", "50"= "#9EBCDA", "75"= "#8C96C6",
                       "100"= "#8C6BB1", "125"= "#88419D", "150"= "#810F7C", "175"= "#4D004B")
        tgh25 <- "NPK 25:10:10 (kg/ha)"
      }else{
        NPKcols25 <- c("0"="#FFFFFF","10"= "#BFD3E6", "20"= "#9EBCDA", "30"= "#8C96C6",
                       "40"= "#8C6BB1", "50"= "#88419D", "60"= "#810F7C", "70"= "#4D004B")
        tgh25 <- "NPK 25:10:10 (kg/acre)"
      }
      
      
      npk25 <- unique(AOIMap3$NPK251010)
      kev <- as.character(npk25[order(npk25)])
      AOIMap3$NPK251010 <- factor(AOIMap3$NPK251010)
      levels(AOIMap3$NPK251010) <- kev
      
      #ggplot NPK
      ggNPK25 <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK251010), col="darkgrey") +
        scale_fill_manual(values = NPKcols25, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
       # geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tgh25) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      ### NPK15:20:20 palette <- brewer.pal(9,"YlOrBr")
      if(unit == "ha"){
        NPKcols15 <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                       "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
        tgh15 <- "NPK 15:20:20 (kg/ha)"
      }else{
        NPKcols15 <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                       "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
        tgh15 <- "NPK 15:20:20 (kg/acre)"
      }
      
      print(Selection3)
      npk15 <- unique(AOIMap3$NPK152020)
      kev <- as.character(npk15[order(npk15)])
      AOIMap3$NPK152020<- factor(AOIMap3$NPK152020)
      levels(AOIMap3$NPK152020) <- kev
      
      #ggplot NPK
      ggNPK15 <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK152020), col="darkgrey") +
        
        scale_fill_manual(values = NPKcols15, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
        # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
       # geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tgh15) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      ### NPK12:30:17 palette <- brewer.pal(9,"YlOrBr")
      if(unit == "ha"){
        NPKcols12 <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                       "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
        tgh12 <- "NPK 12:30:17 (kg/ha)"
      }else{
        NPKcols12 <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                       "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
        tgh12 <- "NPK 12:30:17 (kg/acre)"
      }
      
      
      npk12 <- unique(AOIMap3$NPK123017)
      kev <- as.character(npk12[order(npk12)])
      AOIMap3$NPK123017  <- factor(AOIMap3$NPK123017)
      levels(AOIMap3$NPK123017 ) <- kev
      
      #ggplot NPK
      ggNPK12 <- ggplot(AOIMap3) +
        geom_sf(aes(fill=NPK123017), col="darkgrey") +
        
        scale_fill_manual(values = NPKcols12, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
       # geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tgh12) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      #generate color pallette # brewer.pal(9,"heat")
 
      
      if(unit == "ha"){
        Ydcols <- c( "34"= "#E8FF00FF", "32"="#A2FF00FF", "30"= "#5DFF00FF", "28"= "#17FF00FF", "26"= "#00FF2EFF", "24"= "#00FF74FF",
                     "22"="#00FFB9FF", "20"= "#00FFFFFF", "18"= "#00B9FFFF", "16"= "#0074FFFF", "14"= "#002EFFFF",
                     "12"="#1700FFFF", "10"= "#5D00FFFF", "8"= "#A200FFFF", "6"= "#E800FFFF", "4"= "#FF00D1FF",
                     "2"= "#FF008BFF", "0"= "#FFFFFF")
        tgy <- "Yield increase (t/ha)"
      }else{
        Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                    "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                    "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                    "1"= "#FF008BFF", "0"= "#FFFFFF")
        tgy <- "Yield increase (t/acre)"
      }
      
      Ysclae <- unique(AOIMap3$dY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$dY)
      levels(AOIMap3$dY) <- keY
      
      #Yield ggplot
      ggYield <- ggplot(AOIMap3) +
        geom_sf(aes(fill=dY), col="darkgrey") +
        
        scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ghRegions, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
        #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
        #geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tgy) +
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
     # print(ggNPK11, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggNPK12, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
     # print(ggNPK25, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggNPK15, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
     
      print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      dev.off()
      
      # print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
      # print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      # print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
      # print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
      
      # #-------------------------------------------------------------------------
      # #front page dynamic tmap
      # #-------------------------------------------------------------------------
      # 
 
      
      #reactive selection of variable to view
      filt_select <- reactive({
        print(Selection3)
        if (Selection3 == "Urea rate"){
          filt_select <- "Urea rate"
        }else if (Selection3 == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else if (Selection3 == "NPK 11:22:21 rate"){
          filt_select <- "NPK 11:22:21 rate"
        }else if (Selection3 == "NPK 15:20:20 rate"){
          filt_select <- "NPK 15:20:20 rate"
        }else if (Selection3 == "NPK 25:10:10 rate"){
          filt_select <- "NPK 25:10:10 rate"
        }else if (Selection3 == "NPK 12:30:17 rate"){
          filt_select <- "NPK 12:30:17 rate"
          
        }
        
      })
      
      # choices = c("NPK 15:15:15 rate", "Expected yield response", "Urea rate"),
      # choices = c("NPK 17:17:17 rate", "DAP rate", "Expected yield response", "Urea rate"),
      
      #show map based on selection of variable but retaining single name
      
      #filter by variable selected and unit for color pallette
      observeEvent(filt_select(), {
        if (filt_select() == "Urea rate"){
          
          ureacols <- reactive({
            
            if(unit == "ha"){
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/ha)"
            }else {
              ureacols <- c("#FFFFFF", "#E5F5E0",  "#C7E9C0",  "#A1D99B",  "#74C476",
                            "#41AB5D",  "#238B45", "#006D2C",  "#00441B")
              tturea <- "Urea (kg/acre)"
            }
          })
          
          #reactive legend title
          tturea <- reactive({
            
            if(unit == "ha"){
              
              tturea <- paste("Recommended urea rate(kg/ha)")
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
          
          print(country)
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
                             tm_text(text = "ADM2_EN")
                           sm1
                           
                           
                           
                         })
                       })
        }else if(filt_select() == "NPK 25:10:10 rate"){
               ttnpk25 <- reactive({
            
            if(unit == "ha"){
              
              ttnpk25<- paste("Recommended NPK 25:10:10 rate (kg/ha)")
            }else {
              
              ttnpk25 <- paste("Recommended NPK 25:10:10 rate (kg/acre)")
            }
          })
          
          
          
          npk25 <- unique(AOIMap3$NPK251010)
          kev <- as.character(npk25[order(npk25)])
          AOIMap3$NPK251010 <- factor(AOIMap3$NPK251010)
          levels(AOIMap3$NPK251010) <- kev
          
          #npk plot
          observeEvent(ttnpk25(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK251010",
                               title = ttnpk25(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Reds")+
                             tm_text(text = "ADM2_EN")
                           
                           sm3
                           
                         }) 
                       })
          
        }else if(filt_select() == "NPK 11:22:21 rate"){
          ttnpk11 <- reactive({
            
            if(unit == "ha"){
              
              ttnpk11 <- paste("Recommended NPK 11:22:21 rate (kg/ha)")
            }else {
              
              ttnpk11 <- paste("Recommended NPK 11:22:21 rate(kg/acre)")
            }
          })
          
             
          npk11 <- unique(AOIMap3$NPK112221 )
          kev <- as.character(npk11[order(npk11)])
          AOIMap3$NPK112221  <- factor(AOIMap3$NPK112221 )
          levels(AOIMap3$NPK112221 ) <- kev
          
          #npk plot
          observeEvent(ttnpk11(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm5 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK112221",
                               title = ttnpk11(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Purples")+
                             tm_text(text = "ADM2_EN")
                           
                           sm5
                           
                         }) 
                       })
  
          
        }else if(filt_select() == "NPK 12:30:17 rate"){
          ttnpk12 <- reactive({
            
            if(unit == "ha"){
              
              ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/ha)")
            }else {
              
              ttnpk12 <- paste("Recommended NPK 12:30:17 rate (kg/acre)")
            }
          })
          
          
          
          npk12 <- unique(AOIMap3$NPK123017 )
          kev <- as.character(npk12[order(npk12)])
          AOIMap3$NPK123017  <- factor(AOIMap3$NPK123017 )
          levels(AOIMap3$NPK123017 ) <- kev
          
          #npk plot
          observeEvent(ttnpk12(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK123017",
                               title = ttnpk12(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "YlGnBu")+
                             tm_text(text = "ADM2_EN")
                           
                           sm4
                           
                         }) 
                       })
        }else if(filt_select() == "NPK 15:20:20 rate"){
          ttnpk15 <- reactive({
            
            if(unit == "ha"){
              
              ttnpk15 <- paste("Recommended NPK 15:20:20 rate (kg/ha)")
            }else {
              
              ttnpk15 <- paste("Recommended NPK 15:20:20 rate(kg/acre)")
            }
          })
          
          
          npk15 <- unique(AOIMap3$NPK152020)
          kev <- as.character(npk15[order(npk15)])
          AOIMap3$NPK152020<- factor(AOIMap3$NPK152020)
          levels(AOIMap3$NPK152020) <- kev
          
          #npk plot
          observeEvent(ttnpk15(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           sm6 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "NPK152020",
                               title = ttnpk15(),
                               #tm_borders(lwd = 1, col = "black", alpha = .5) +
                               # breaks = c(100, 110, 120, 130),
                               # labels = c("Low", "Medium", "High"),
                               palette = "Oranges")+
                             tm_text(text = "ADM2_EN")
                           
                           sm6
                           
                         }) 
                       })
     
          
 
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({
            
            if(unit == "ha"){
              
              ttha <- paste("Recommended yield increase (t/ha)")
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
                               palette = "Blues")+
                             tm_text(text = "ADM2_EN")
                           
                           sm3
                           
                         })
                       })
          
          
          
        }
        
        
      })
      
      
      
      print(Selection3)
      if ( usecase == "Fertilizer Recommendation" & unit == "acre"){
        
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
        
      }else if(usecase == "Fertilizer Recommendation" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      }else if (!is.null(input$btn_go) & usecase == "Scheduled Planting" & unit == "acre"){
        #download acre printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(!is.null(input$btn_go)  & usecase == "Scheduled Planting" & unit == "ha"){
        #download hectare printable guides
        output$downloadDatasp <- 
          downloadHandler(
            filename <- function() {
              paste("SP Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Scheduled Planting and Harvest Cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          )  
        
        
      }  
      
      
      
      
      
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
  #})
  
}


#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
