#setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard - Copy (6)")

#######################################################################################
## Read the GIS layers
#######################################################################################

TownsNG <- readOGR(dsn = ".", layer = "Places_towns")
RiversNG <- readOGR(dsn = ".", layer = "Rivers")


boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")
ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")


boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")


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