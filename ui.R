library(shinydashboard)
library(dplyr)
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
library(shinyalert)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
library(grid)
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
library(shinycssloaders)
library(shinybusy)


### SHINY UI ###
ui <- bootstrapPage(
 
             navbarPage(windowTitle ="Tanzania",
                        title = div(img(src = "https://akilimo.sirv.com/images/tz.PNG","Tanzania", height = "60", style="margin-top: -14px;
                                padding-right:10px;
                               padding-bottom:10px")),
                          
                        theme = shinytheme("sandstone"), collapsible = TRUE,inverse = TRUE,id="nav",
                          
              tags$head(tags$style("#shiny-modal img { max-width: 50%;display: block; margin-left: auto; margin-right: auto; }")),
    
             #conditionalPanel(condition = ("input.ok > 0 & input.use == 'Fertilizer Recommendation'"),

             tabPanel("Use case mapper",
                      shinyalert::useShinyalert(), 
                       div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          #leafletOutput("mymap", width="100%", height="100%"),
                          #shinyjs::useShinyjs(),
                          #withSpinner(tmapOutput(outputId = "ureaplot", width="100%", height="100%"), type = 2),
                          tmapOutput(outputId = "tmapplot", width="100%", height="100%"),
                          # tmapOutput(outputId = "npkplot", width="100%", height="100%"),
                          # tmapOutput(outputId = "yieldplot", width="100%", height="100%"),
                      

                          absolutePanel(id = "controls", class = "panel panel-default",
                                        tags$head(includeCSS("styles.css")),
                                        top = 75, left = 70, width = 320, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                      uiOutput("region"),
                      
                    
                      uiOutput("unit_loc"),
                      
                       conditionalPanel(
                        condition = "input.unit_loc == 'hectare'",
                      uiOutput("FCY_ha")),
                      
                      conditionalPanel(
                        condition = "input.unit_loc == 'acre'",
                      uiOutput("FCY_acre")
                      ),
                      
                      uiOutput("plntmth"),
                      
                      uiOutput("selection"),
                      
                      uiOutput("costs")
  
                      )),
                      
                      #span(tags$i(h4("Give price information here.")), style="color:#045a8d"),
                      conditionalPanel(
                      condition =  "input.costs == 'Yes' ",
                      absolutePanel(id = "controls", class = "panel panel-default",
                                    bottom = 75, left = 405, width = 250, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
 
                                    uiOutput("CassavaPrice"),
                                    uiOutput("NPK151515Price"),
                                    uiOutput("NPK171717Price"),
                                    uiOutput("DAPPrice"),
                                    uiOutput("UreaPrice"),
                                    
                                shinyalert::useShinyalert(),                 
                                    
                                    
                      )),
                    
                                    
            
                          
                          absolutePanel(id = "logo", class = "card", bottom = 100, right = 200, width = 200, fixed=TRUE, draggable = FALSE, 
                                        tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 290, width = 300
                          ),
                          tags$a(href="https://akilimo.org", "Learn more...")
                          
                          ),
                     
                      absolutePanel(id = "go", class = "panel panel-default",
                                    bottom = 5, left = 405, width = 150, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                      uiOutput("btn_go")
                      )),
                      
                      #conditionalPanel(condition = "input.buton > 0", p("I'm a dashboard"))
                      conditionalPanel(condition = ("input.btn_go > 0"),
                      absolutePanel(id = "fr", class = "panel panel-default",
                                    bottom = 5, right = 100, width = 200, fixed=TRUE,
                                    draggable = TRUE, height = "auto",
                                    
                                    downloadButton("downloadDatafr", "Download printable guide")
                                    #uiOutput("downloadDatafr")
                      ),
                      
                     
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "100",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')",
                                                                       "https://twitter.com/ACAI_IITA")))
                          #absolutePanel(uiOutput("btn_go"))https://twitter.com/ACAI_IITA

                      ),
                      
            
    
    

             tabPanel("View maps side by side",width = "100%",
                      
                      textOutput("sidetext"),
                      
                      
                      br(),
                      
                      fluidRow(
                        column(6,
                               downloadButton("downloadData", "Download pdf of maps")
                               
                        ),
                        column(6,
                               #downloadButton("downloadDatafr", "Download printable guide")
                        )
                      ),
                      
                      
                      fluidRow(
                        shinydashboard::box(width = 4,title = "Urea",withSpinner(tmapOutput(outputId = "ureaplot2", width = 450, height = 600))
                        ),
                        
                
                        shinydashboard::box(width = 4,title = "NPK 17:17:17",withSpinner(tmapOutput(outputId = "npkplot_tr", width = 450, height = 600))),
                        
                        
                        shinydashboard::box(width = 4,title = "Yield",withSpinner(tmapOutput(outputId = "yieldplot", width = 450, height = 600))),
                        
                     
                                         shinydashboard::box(width = 4,title = "DAP",withSpinner(tmapOutput(outputId = "dapplot", width = 450, height = 600)))
                        
                      )
             ),

             
            
             tabPanel("View Table",
                      
                      
                      
                     
                                       textOutput("tabletext_tzs"),
                      
                      
                      
                      #h3('The table below specifies the recommended fertilizer application rates by LGA and month of planting, as well as the expected root yield response. '),
                      conditionalPanel(condition="input.costs == 'No'",
                                       box(class = "mybg",
                                           br(),
                                           width = NULL, status = 'primary',
                                           DT::dataTableOutput('mytable', width = "100%")
                                           
                                       )
                      ),
                      conditionalPanel(condition="input.costs == 'Yes'",
                                       box(class = "mybg",
                                           br(),
                                           width = NULL, status = 'primary',
                                           DT::dataTableOutput('mytable2', width = "100%")
                                           
                                       )
                      )
                      
                      
             ),
             
             tabPanel(
               uiOutput("tabers")
             ),
             
            # conditionalPanel(condition = ("input.ok > 0 & input.use == 'Scheduled Planting'"),
             
             tabPanel("Expected yield for different planting and harvest schedules",id = "spt",

                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     
                                   
                                     pickerInput("regionSP", "Select region", choices = c("Geita", "Kagera","Kigoma","Lindi", "Mara", "Mtwara", "Mwanza","Pwani", "Shinyanga",  
                                                                                        "Simiyu", "Tanga","Zanzibar South and Central"),
                                                 selected = NULL,
                                                 multiple = TRUE,
                                                 options = pickerOptions(maxOptions = 1)),
                                     
                                     
                                     pickerInput("plntmthSP", "Select planting month",
                                                 choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                                             "October", "November", "December"),
                                                 selected = NULL,
                                                 multiple = TRUE,
                                                 options = pickerOptions(maxOptions = 1)),
                                     
                                            pickerInput("unit_locSP", "Select unit of land",
                                                       choices = c("acre", "hectare"),
                                                  selected = NULL,
                                                  multiple = TRUE,
                                                  options = pickerOptions(maxOptions = 1)),
 
                                    

                        
                        br(),
                        conditionalPanel(condition="input.unit_locSP == 'acre'| input.unit_locSP == 'hectare'",
                                         absolutePanel(id = "sp2", class = "card", bottom = 260, left = 120, width = 70, fixed=TRUE, draggable = TRUE, 
                                                       
                                                       
                                                       actionButton("btn_SP", "Generate Table", icon("table"),
                                                                    style="color: #fff; background-color: green; border-color: #2e6da4"))),
                        br(),
                        
                        conditionalPanel(condition = ("input.btn_SP > 0"),
                                         absolutePanel(id = "sp3", class = "card", bottom = 200, left = 85, width = 70, fixed=TRUE, draggable = TRUE, 
                                                       
                                                       
                                                       downloadButton("downloadDatasp", "Download Printable guide",height = 100, width = 150)
                                         ),
                                         
                                         
                                         absolutePanel(id = "logo2", class = "card", bottom = 0, left = 30, width = 70, fixed=TRUE, draggable = TRUE, 
                                                       
                                                       br(),
                                                       br(),
                                                       br(),
                                                       br(),
                                                       tags$img(src='https://akilimo.sirv.com/images/akilimo_icon.png',height = 200, width = 200
                                                       )
                                         )
                                         
                                         
                           
                             )            
                                     
                        ),
                        
                   
                                 mainPanel(width = 7,
                                  
                              
                                    conditionalPanel(condition = ("input.btn_SP > 0"),
                                                     textOutput("tabletext_sp"),
                                                     
                                                    
                                conditionalPanel(condition = ("input.btn_SP > 0"),
                                    shinydashboard::box( width = NULL,withSpinner(
                                      type = getOption("spinner.type", default = 4),
                                      color="#0dc5c1",
                                      #color = getOption("spinner.color", default = "#0275D8"),
                                      size = getOption("spinner.size", default = 1),

                                           DT::dataTableOutput('SPtable', width = "125%")

                                       ))
                                )
                              )
                      ))),
             
             
  

    tags$style(HTML(".navbar-header { width:45% }
                   .navbar-brand { width: 45%; font-size: 23px}")) # cen    
                      
             
  )        
)


