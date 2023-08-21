
setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH")

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
Agg_plantingMonth_SP <- function(ds,country, adminName, FCYName, unit, mainDir){
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
  if(unit == "ha"){
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
  
  
  
  subDir <- paste(adminName, FCYName, unit, sep="_")
  if(!dir.exists(file.path(mainDir, subDir))){
    dir.create(file.path(mainDir, subDir))
    setwd(paste(mainDir, subDir, sep="/"))
  }else{
    setwd(paste(mainDir, subDir, sep="/"))
  }
  
  for(plm in unique(CY_wly_average$plm)){
    SP_plDate <- CY_wly_average[CY_wly_average$plm == plm, ]
    SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2),]
    fname <- paste(adminName, "_SP_paperBased_ton_", plm, ".csv",sep="")
    SP_plDate <- SP_plDate[order(SP_plDate$NAME_1, SP_plDate$NAME_2), ]
    write.csv(SP_plDate, fname, row.names = FALSE)
  }
  return(CY_wly_average)
  
}



#' @description : reads the individual csv files and make a pdf for each. 
EditMakePdf <- function(country, adminName, FCYName, unit){
  setwd(paste("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH/SP/",country, "/", adminName, "_", FCYName, "_", unit , sep=""))
  # setwd(paste("D:/ACAI_Wrapper/cloud_compute/SP/",aggs, "/",country, "/", adminName, "_", FCYName, "_", unit , sep=""))
  listcsv <- list.files(getwd(),pattern = paste(adminName, "_SP_paperBased_ton_", sep=""))
  
  for(files in c(listcsv, listcsv[1])){
    csvtables <- read.csv(files)
    plantMonth <- unique(csvtables$plm)
    csvtables <- csvtables[, c("NAME_1", "NAME_2","Harvest_8","Harvest_9","Harvest_10", "Harvest_11",
                               "Harvest_12", "Harvest_13", "Harvest_14","Harvest_15","plm" )]
    csvtables <- csvtables[order(csvtables$NAME_1, csvtables$NAME_2), ]
    
    rrtz <- c("REGION", "DISTRICT")
    rrng <- c("STATE", "LGA")
    
    if(plantMonth == "January"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(9:12, 1:4)], "plantingMonth")
    }else if(plantMonth == "February"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(10:12, 1:5)], "plantingMonth")
    }else if(plantMonth == "March"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(11:12, 1:6)], "plantingMonth")
    }else if(plantMonth == "April"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(12, 1:7)], "plantingMonth")
    }else if(plantMonth == "May"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(1:8)], "plantingMonth")
    }else if(plantMonth == "June"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(2:9)], "plantingMonth")
    }else if(plantMonth == "July"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(3:10)], "plantingMonth")
    }else if(plantMonth == "August"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(4:11)], "plantingMonth")
    }else if(plantMonth == "September"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(5:12)], "plantingMonth")
    }else if(plantMonth == "October"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(6:12, 1)], "plantingMonth")
    }else if(plantMonth == "November"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(7:12, 1:2)], "plantingMonth")
    }else if(plantMonth == "December"){
      colnames(csvtables) <- c(switch(country, NG={rrng}, TZ={rrtz}), month.name[c(8:12, 1:3)], "plantingMonth")
    }
    
    csvtables <- subset(csvtables, select=-c(plantingMonth))
    cols <- names(csvtables)[3:10]
    require(dplyr)
    csvtables <- csvtables %>% mutate_at(vars(cols), funs(round(., 1)))
    
    if(nrow(csvtables)>0){
      fileNameR <- paste(adminName, "_SP_table","_", plantMonth, ".pdf", sep="")
      pdfTables(FERTtABLE = csvtables, plantMonth, region=adminName, fileNameR=fileNameR, unit)
    }
  }
  
  
  if(unit == "ha"){
    uu = "t/ha"
  } else {
    uu = "t/acre"
  }
  
  fileName_p <- paste(adminName,  ".pdf", sep="")
  a = paste(lgaGroups = adminName)
  if(country == 'NG'){
    b = paste("Cassava root yield depends on planting and harvest months.
This tool contains tables with advice for planting and harvest months
for the scheduled planting use case. For every state and LGA,a table
is generated by planting month, indicated as a title of the table.
The expected root yield in ", uu ," for harvest in 8, 9, 10, 11,
12, 13, 14 and 15 months after planting are given as columns
in the tables.", sep=" ")
  }else{
    b = paste("Cassava root yield depends on planting and harvest months.
This tool contains tables with advice for planting and harvest months
for the scheduled planting use case. For every region and district,
a table is generated by planting month, as indicated in the title
of the table. The expected root yield in ", uu ," for harvest in
8, 9, 10, 11, 12, 13, 14 and 15 months after planting are given
as columns in the tables.", sep=" ")
  }
  pdf(fileName_p, paper="a4", pagecentre=FALSE, width=12,height=14)
  plot(NA, xlim=c(0.2,6), ylim=c(0,6), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  text(2,5,a, pos=4, cex=3.5)
  text(0,0.5,b, pos=4, cex=1.1)
  dev.off()
  
}

#' @describeIn : converting csv to pdf
pdfTables <- function(FERTtABLE, plantMonth, region, fileNameR, unit){
  row.names(FERTtABLE) = NULL
  pdf(fileNameR, width = 12, height = 12)
  if(region %in% c("Oyo", "Osun", "Delta", "Benue", "Imo", "Akwa Ibom")){
    pdf.options(paper = "a4")
  }else{
    pdf.options(paper = "a4r")
  }
  
  
  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5), ncol=10, nrow=nrow(FERTtABLE), byrow=TRUE)
  if(region %in% c("Oyo", "Osun", "Delta", "Benue", "Imo", "Akwa Ibom")){
    tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
                          colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.8)),
                          rowhead = list(fg_params=list(cex = 0.7)))
  }else{
    tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
                          colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                          rowhead = list(fg_params=list(cex = 0.9)))
  }
  
  table <- tableGrob(FERTtABLE, rows = NULL, theme = tt1)
  if(unit == "ha"){
    title <- textGrob(paste("Expected yield in t/ha when you plant in ",plantMonth, " and harvest in ...", sep=""), gp = gpar(fontsize = 14))
  }else{
    title <- textGrob(paste("Expected yield in t/acre when you plant in ",plantMonth, " and harvest in ...", sep=""), gp = gpar(fontsize = 14))
  }
  
  padding <- unit(0.4,"line")
  table <- gtable_add_rows(
    table, heights = grobHeight(title) + padding, pos = 0
  )
  table <- gtable_add_grob(
    table, list(title),
    t = 1, l = 1, r = ncol(table)
  )
  grid.newpage()
  grid.draw(table)
  dev.off()
  
}

unit <- "acre"

#' @description join he first page and all the pdfs of different months
joinPdfs <- function(country, adminName, FCYName, unit){
  setwd(paste("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH/SP/",country, "/", adminName, "_", FCYName, "_", unit, sep=""))
  # setwd(paste("D:/ACAI_Wrapper/cloud_compute/SP/",aggs, "/",country, "/", adminName, "_", FCYName, "_", unit,sep=""))
  firstPage <- paste(adminName, ".pdf", sep="")
  fname <- paste(adminName, "_joined_",unit,".pdf", sep="")
  listcsv <- list.files(getwd(),pattern = paste(adminName, "_SP_paperBased_ton_", sep=""))
  mNames <- c()
  for(files in listcsv){
    csvtables <- read.csv(files)
    plantMonth <- unique(csvtables$plm)
    mNames <- c(mNames, as.character(plantMonth))
  }
  AM <- month.name[month.name %in% mNames]
  pdf_combine(c(firstPage , paste(paste(adminName, "SP_table", AM, sep="_"), ".pdf", sep="")), output = fname)
}




############################################################################################################
## NIGERIA
############################################################################################################
############################################################################################################
## NG: read data
############################################################################################################
#setwd("D:/ACAI_Wrapper/cloud_compute/SP")
setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH")
WLY_CY_FCY1_LGA <- readRDS("WLY_CY_fcy1_NG_2020_FW_everypoint.RDS")

############################################################################################################
##  NG: add week nrs
############################################################################################################
PHdate <- Planting_HarvestDate(country = "NG")
CY_WLY_coord <- merge(WLY_CY_FCY1_LGA, PHdate[, c("st","weekNr")], by.x="plantingDate", by.y="st")
head(CY_WLY_coord)
adminName <- unique(CY_WLY_coord$NAME_1)
country <- "NG"
FCYName <- "FCY_1"

############################################################################################################
###  NG: creating directories to save by country, FCY and ha/acre
############################################################################################################
mainDir <- paste("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard_GH/SP/", country, sep="")
for (aname in adminName){
  subDir <- paste(aname, FCYName, "ha", sep="_")
  dir.create(file.path(mainDir, subDir))
  subDir <- paste(aname, FCYName, "acre", sep="_")
  dir.create(file.path(mainDir, subDir))
}

############################################################################################################
##  NG: by Region/State and district/LGA it averages out current yield, WLY and by harvest month writes the file to the sub dir
############################################################################################################

#this creates table
for (AN in unique(CY_WLY_coord$NAME_1)){
  CY_WLY_coord_AN <- CY_WLY_coord[CY_WLY_coord$NAME_1 == "Oyo", ]
  Agg_plant_AN <- Agg_plantingMonth_SP(ds=CY_WLY_coord_AN, country = "NG", adminName = "Oyo", FCYName="FCY_1", unit="ha",mainDir)
  
}

for (AN in unique(CY_WLY_coord$NAME_1)){
  CY_WLY_coord_AN <- CY_WLY_coord[CY_WLY_coord$NAME_1 == AN, ]
  Agg_plant_AN <- Agg_plantingMonth_SP(ds=CY_WLY_coord_AN, country = "NG", adminName = AN, FCYName="FCY_1", unit="acre",mainDir)
  
}

############################################################################################################
##  NG: rearrange rename and make gwo pdfs: one is for the recommnedationa dn the other as a cover page giving 
## general info. for the shiny app the recommendation need to displayed but when a client downloads the file the 
## cover page pdf alos need to be attached whcih can be done using the function joinpdf
############################################################################################################

#this makes a pdf of the same
for (AN in unique(CY_WLY_coord$NAME_1)){
  EditMakePdf(country = "NG",adminName = "Oyo", FCYName="FCY_1", unit="ha")
}


for (AN in unique(CY_WLY_coord$NAME_1)){
  EditMakePdf(country = "NG",adminName = AN, FCYName="FCY_1", unit="acre")
}


############################################################################################################
##  NG: join cover page and the 
############################################################################################################

for (AN in unique(CY_WLY_coord$NAME_1)){
  joinPdfs(country = "NG",adminName = "Oyo", FCYName="FCY_1", unit="ha")
}


for (AN in unique(CY_WLY_coord$NAME_1)){
  joinPdfs(country = "NG",adminName = AN, FCYName="FCY_1", unit="acre")
}



############################################################################################################
## TANZANIA
############################################################################################################
############################################################################################################
## TZ: read data
############################################################################################################
setwd("D:/ACAI_Wrapper/cloud_compute/SP/TZ")
WLY_CY_FCY1_Region <- readRDS("WLY_CY_FCY1_Region_2020_Average.RDS")
head(WLY_CY_FCY1_Region)

unique(WLY_CY_FCY1_Region$NAME_1)
Zanz <- WLY_CY_FCY1_Region[grep("Zanzibar", WLY_CY_FCY1_Region$NAME_1), ]
rest <- WLY_CY_FCY1_Region[-c(grep("Zanzibar", WLY_CY_FCY1_Region$NAME_1)), ]
Zanz$NAME_1 <- as.factor(as.character("Zanzibar") )

WLY_CY_FCY1_Region <- rbind( rest,Zanz)

############################################################################################################
##  TZ: add week nrs
############################################################################################################
PHdate <- Planting_HarvestDate(country = "NG")
PHdate$Zone <- "TZ"

CY_WLY_coord <- merge(WLY_CY_FCY1_Region, PHdate[, c("st","weekNr")], by.x="plantingDate", by.y="st")
head(CY_WLY_coord)
adminName <- unique(CY_WLY_coord$NAME_1)
country <- "TZ"
FCYName <- "FCY_1"

############################################################################################################
###  TZ: creating directories to save by country, FCY and ha/acre
############################################################################################################
mainDir <- paste("D:/ACAI_Wrapper/cloud_compute/SP/", country, sep="")


for (aname in adminName){
  subDir <- paste(aname, FCYName, "ha", sep="_")
  dir.create(file.path(mainDir, subDir))
  subDir <- paste(aname, FCYName, "acre", sep="_")
  dir.create(file.path(mainDir, subDir))
}

############################################################################################################
##  TZ: by Region/State and district/LGA it averages out current yield, WLY and by harvest month writes the file to the sub dir
############################################################################################################
for (AN in unique(CY_WLY_coord$NAME_1)){
  CY_WLY_coord_R <- CY_WLY_coord[CY_WLY_coord$NAME_1 == AN,] 
  Agg_plant_Ebonyi <- Agg_plantingMonth_SP(ds=CY_WLY_coord_R, country = "TZ", adminName = AN, FCYName="FCY_1", unit="ha",mainDir)
  
}

for (AN in unique(CY_WLY_coord$NAME_1)){
  CY_WLY_coord_R <- CY_WLY_coord[CY_WLY_coord$NAME_1 == AN, ]
  Agg_plant_Ebonyi <- Agg_plantingMonth_SP(ds=CY_WLY_coord_R, country = "TZ", adminName = AN, FCYName="FCY_1", unit="acre",mainDir)
  
}

############################################################################################################
##  TZ: rearrange rename and make gwo pdfs: one is for the recommnedationa dn the other as a cover page giving 
## general info. for the shiny app the recommendation need to displayed but when a client downloads the file the 
## cover page pdf alos need to be attached whcih can be done using the function joinpdf
############################################################################################################
for (AN in unique(CY_WLY_coord$NAME_1)){
  EditMakePdf(country = "TZ",adminName = AN, FCYName="FCY_1", unit="ha")
}


for (AN in unique(CY_WLY_coord$NAME_1)){
  EditMakePdf(country = "TZ",adminName = AN, FCYName="FCY_1", unit="acre")
}


############################################################################################################
##  TZ: join cover page and the 
############################################################################################################

for (AN in unique(CY_WLY_coord$NAME_1)){
  joinPdfs(country = "TZ",adminName = AN, FCYName="FCY_1", unit="ha")
}


for (AN in unique(CY_WLY_coord$NAME_1)){
  joinPdfs(country = "TZ",adminName = AN, FCYName="FCY_1", unit="acre")
}



