#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/PaperbasedDashboard_NG -testSP")

require(gtools)
library(rgdal)
library(raster)
library(dismo)
library(rgeos)
library(ggspatial)
require(plyr)
library(gtable)
require(qpdf)
library(gridExtra)
library(grid)
require(tidyr)

############################################################################################################
## All the functions
############################################################################################################
#'SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
#' @return :   RFY: root fresh yield in the same units as root DM yield input
#' @description : Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#' @param :  HD: harvest date (Date format)
#' @param :  RDY: root dry matter yield (user's units)
#' @param :  country = c("NG", "TZ")
getRFY <- function(HD, RDY, country = c("NG", "TZ"), fd){
  d <- HD
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RFY <- RDY / DC * 100
  return(RFY)
}

#' @param country: "NG" or "TZ"
#' get the planting and harvest dates
#' @return a data frame with st: planting week,en: the maximum number of dates after planting before harvest,
#' set at 455 days (15 MAP). weekNr: the week number of planting dates, Zone: for TZ lake, east and south and for NIgeria NG
#' @example PHdate <- Planting_HarvestDate(country="NG")
Planting_HarvestDate <- function (country){
  if(country == "TZ"){
    weeknr_LZ <- c(c(1:5), c(9:22), c(31:52))
    Pl_LZ <- c(seq(1, 31, 7),seq(61, 152, 7), seq(214, 365, 7))
    Harvest_LZ <- Pl_LZ + 454
    
    weeknr_EZ <- c(c(1:18), c(35:52))
    Pl_EZ <- c(seq(1, 121, 7), seq(245, 365, 7))
    Harvest_EZ <- Pl_EZ + 454
    
    weeknr_SZ <- c(c(1:9), c(31:52))
    Pl_SZ <- c(seq(1, 60, 7), seq(214, 365, 7))
    Harvest_SZ <- Pl_SZ + 454
    
    PH_date_LakeZone <- data.frame(st=Pl_LZ, en = Harvest_LZ, weekNr = weeknr_LZ) ## 41 days
    PH_date_EastZone <- data.frame(st=Pl_EZ, en = Harvest_EZ, weekNr = weeknr_EZ) ##36 days
    PH_date_SouthZone <- data.frame(st=Pl_SZ, en = Harvest_SZ, weekNr = weeknr_SZ) ##31 days
    PH_date_LakeZone$Zone <- "lake"
    PH_date_EastZone$Zone <- "east"
    PH_date_SouthZone$Zone <- "south"
    PH <- rbind(PH_date_LakeZone, PH_date_EastZone, PH_date_SouthZone)
  }else if (country ==  "NG"){
    PH <- data.frame(st=seq(1, 365, 7), en = (seq(1, 365, 7) + 454), weekNr = seq(1:53))
    PH$Zone <- "NG"
  }
  return(PH)
}

#' take the mean of WLY and CY from FCY1 by LGA/District within STATE/REGION and planting month and monthly harvest 8 - 15 MAP
#' @param ds : output of function addCoord

Agg_plantingMonth_SP <- function(ds,country, adminName, FCYName, unit){
  ds$plm <- as.factor(ds$weekNr)
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


  CY_WLY_8 <- droplevels(ds[ds$DaysAfterPlanting %in% c(214,221,228,235,242), ])#Aug
  CY_WLY_9 <- droplevels(ds[ds$DaysAfterPlanting %in% c(249,256,263,270), ])#sep
  CY_WLY_10 <- droplevels(ds[ds$DaysAfterPlanting %in% c(277,284,291,298), ])#Oct
  CY_WLY_11 <- droplevels(ds[ds$DaysAfterPlanting %in% c(305,312,319, 326), ])#nov
  CY_WLY_12 <- droplevels(ds[ds$DaysAfterPlanting %in% c(333,340,347,354,361), ])#dec
  CY_WLY_13 <- droplevels(ds[ds$DaysAfterPlanting %in% c(368,375,382,389), ])#jan
  CY_WLY_14 <- droplevels(ds[ds$DaysAfterPlanting %in% c(396,403,410,417), ])#feb
  CY_WLY_15 <- droplevels(ds[ds$DaysAfterPlanting %in% c(424,431,438,445,452), ])#mar


  CY_wly_average <- NULL
  if(unit == "hectare"){
    for(j in c(8:15)){
      av <- unique(ddply(eval(parse(text = paste("CY_WLY_", j, sep=""))), .(plm, NAME_2, NAME_1), summarize,
                         meanHarvest = mean(((WLY_user + CY_user)/2))))
      # meanHarvest = round(mean(((WLY_user + CY_user)/2)), digits=0)))
      colnames(av) <- c("plm","NAME_2", "NAME_1", paste("Harvest_",j, sep=""))
      if(j == 8){
        CY_wly_average <-  av
      }else{
        CY_wly_average <- merge(CY_wly_average, av, by=c("plm", "NAME_2", "NAME_1"))
      }
    }
  }else{
    for(j in c(8:15)){
      av <- unique(ddply(eval(parse(text = paste("CY_WLY_", j, sep=""))), .(plm, NAME_2, NAME_1), summarize,
                         #meanHarvest = round(mean(((WLY_acre + CY_acre)/2)), digits=0)))
                         meanHarvest = mean(((WLY_acre + CY_acre)/2))))
      colnames(av) <- c("plm","NAME_2", "NAME_1", paste("Harvest_",j, sep=""))
      if(j == 8){
        CY_wly_average <-  av
      }else{
        CY_wly_average <- merge(CY_wly_average, av, by=c("plm", "NAME_2", "NAME_1"))
      }
    }

  }




  return(list(CY_wly_average))

}



