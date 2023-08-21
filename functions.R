# LGAMaps(plantMonth="January",  cities = c("Calabar", "Makurdi"), lgaGroups = "Benue_Cross River",
#         LGApoints = Benue_CR, stateLabel = Benue_CRlabel, textangle=0, unit="acre", couple = "Two")



LGAMaps <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
  
  if(couple == "Two"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
  }
  
  if(couple == "Three"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
  }
  
  plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
  
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
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("LGA", "Urea", "NPK15_15_15","dY", "LGAdY")]),by.x="NAME_2" ,by.y="LGA")
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("STATE","LGA", "Urea", "NPK15_15_15", "LGAdY", "month")]))
  tt <- tt[order(tt$STATE, tt$LGA), ]
  write.csv(tt, fileNameCsv, row.names = FALSE)
  
  AOIMap3 <- st_as_sf(AOIMap2)
  
  # Ureapalette <- brewer.pal(9,"Greens")
  # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])

  ureasclae <- unique(AOIMap3$Urea)
  keU <- as.character(ureasclae[order(ureasclae)])
  AOIMap3$Urea <- factor(AOIMap3$Urea)
  levels(AOIMap3$Urea) <- keU
  
  
  mopsclae <- unique(AOIMap3$NPK15_15_15)
  kev <- as.character(mopsclae[order(mopsclae)])
  AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
  levels(AOIMap3$NPK15_15_15) <- kev
  
  Ysclae <- unique(AOIMap3$dY)
  keY <- as.factor(Ysclae[order(Ysclae)])
  AOIMap3$dY <- factor(AOIMap3$dY)
  levels(AOIMap3$dY) <- keY
  
  return(list(AOIMap3, AOIMapS, LGAnames, towns,crop_ngstate))
}
  
  # require(ggrepel) 
  # 
  # ggUrea <- ggplot(AOIMap3) +
  #   geom_sf(aes(fill=Urea), col="grey30") +
  #   scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
  #   geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
  #   geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
  #   #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
  #   geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
  #   geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
  #   geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
  #   geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
  #   annotation_scale(location = "br", width_hint = 0.3, line_width = 0.4) +
  #   annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
  #                          style = north_arrow_fancy_orienteering) +
  #   # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
  #   #                        style = north_arrow_fancy_orienteering) +
  #   # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
  #   xlab("") + ylab("") +
  #   ggtitle(tt) +
  #   theme_bw() +
  #   theme(legend.position="right", legend.title=element_blank(),
  #         plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
  #         axis.text = element_text(size=8)) 
  # 
  # 
  # ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  # if(unit == "ha"){
  #   NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
  #                "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
  #   tt <- "NPK 15-15-15 (kg/ha)"
  # }else{
  #   NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
  #                "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
  #   tt <- "NPK 15-15-15 (kg/acre)"
  # }
  # 
  # 
  # mopsclae <- unique(AOIMap3$NPK15_15_15)
  # kev <- as.character(mopsclae[order(mopsclae)])
  # AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
  # levels(AOIMap3$NPK15_15_15) <- kev
  # 
  # require(plotly)
  # ggNPK <- ggplot(AOIMap3) +
  #   geom_sf(aes(fill=NPK15_15_15), col="grey30") +
  #   scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
  #   geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
  #   geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
  #   geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
  #   # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
  #   #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
  #   geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
  #   geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
  #   xlab("") + ylab("") +
  #   ggtitle(tt) +
  #   theme_bw() +
  #   theme(legend.position="right", legend.title=element_blank(),
  #         plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
  #         axis.text = element_text(size=8))
  # 
  # 
  # if(unit == "ha"){
  #   tt <- "Yield increase (t/ha)"
  #   Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
  #               "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
  #               "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
  # }else{
  #   tt <- "Yield increase (t/acre)"
  #   Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
  #               "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
  #               "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
  # }
  # 
  # Ysclae <- unique(AOIMap3$dY)
  # keY <- as.factor(Ysclae[order(Ysclae)])
  # AOIMap3$dY <- factor(AOIMap3$dY)
  # levels(AOIMap3$dY) <- keY
  # 
  # ggYield <- ggplot(AOIMap3) +
  #   geom_sf(aes(fill=dY), col="grey30") +
  #   scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
  #   geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
  #   geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
  #   geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
  #   #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
  #   #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
  #   geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
  #   geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
  #   xlab("") + ylab("") +
  #   ggtitle(tt) +
  #   theme_bw() +
  #   theme(legend.position="right", legend.title=element_blank(),
  #         plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
  #         axis.text = element_text(size=8))
  # 
  # 
  # fileName <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
  # pdf(fileName, onefile = TRUE, height = 14, width=12)
  # #pdf.options(paper = "a4")
  # grid.newpage()
  # pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5,0.8), "null"))))   
  # grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  # print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  # print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  # #print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  # print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  # dev.off()
  # 
  # 
  # 
  # 
