

#' ---
#' title: "Analysis of Coastal Forested Wetland Change"
#' author: "Elliott White Jr."
#' date: "June 12,2021"
#' output: github_document
#' ---
#'


#' This script is used to calculate drainage density for different flow types for the
#' subwatersheds of interest on the north atlantic coastal plain. 
#' The watershed data was downloaded from NHDPlus which can be accessed:
#' https://viewer.nationalmap.gov/basic/?basemap=b1&category=nhd&title=NHD%20View#startUp
#' 

#' Locally the files are found here: 
setwd("your file path")


### Install Packages
{
  install.packages('rjson')
  install.packages('rsq')
  install.packages('readr')
  install.packages('alluvial')
  install.packages('RColorBrewer')
  install.packages('MuMIn')
  install.packages('Metrics')
}

### Load Package
{
  library(rjson)
  library(rsq)
  library(readr)
  library(alluvial)
  library(RColorBrewer)
  library(MuMIn)
  library(Metrics)
}


###########################################################################
### Importing data, which should be stored in the working directory.    ###
### IMPORT DICTIONARIES FROM GEE. THEY SHOULD BE SAVED AS A .json FILE. ###
###########################################################################
{
  ### Change from 1996 to 2016 for different regions
  
  nacpChange <- as.data.frame(fromJSON(file = 'nacpChange1996_2016.json'))
  acpChange <- as.data.frame(fromJSON(file = 'acpChange1996_2016.json'))
  ngomChange <- as.data.frame(fromJSON(file = 'ngomChange1996_2016.json'))
  mapGain <- as.data.frame(fromJSON(file = 'mapGain.json'))
  
  
  ### Frequency Histograms
  # The 0.0009 converts the count of pixels to km^2
  swampFrequency <- as.data.frame(fromJSON(file = 'swampFrequency.json'))*.0009
  swampGain <- as.data.frame(fromJSON(file = 'swampGain.json'))*.0009
  waterFrequency <- as.data.frame(fromJSON(file = 'waterFrequency.json'))*.0009
  
  
  ### Alluvial Frequencies
  swampAlluvial <- read.csv('alluvialTry.csv')
  swampAlluvial <- swampAlluvial[c(1:10),c(1:3)]
  
  ### Fragmentation
  ### Files are generated and exported from ArcGIS
  # Neuse-Pamlico and Chowan-Roanoke Subregions
  geo1996 <- read_csv('geometry1996_TableToExcel.csv')
  geo2017 <- read_csv('geometry2017_TableToExcel.csv')
  
  # Palmetto-Peartree Preserve
  palmPear1996 <- read_csv('pptFrag1996_TableToExcel.csv')
  palmPear2017 <- read_csv('pptFrag2017_TableToExcel.csv')
  
  
  ### Modeling
  newCor <- read.csv('newCor.csv', row= 1)
  terrainStuff <- as.data.frame(fromJSON(file = 'terrain.json'))
  newCor$elevation <- terrainStuff[rev(c(17,37,44,46,51,49,43,30,35,42,31,36,38,41)),3]
  newCor$slope <- terrainStuff[rev(c(17,37,44,46,51,49,43,30,35,42,31,36,38,41)),4]
  }
  
#####################
### Create tables ###
#####################
{
  ### Separating the segions based on their coastline connection
  acp <- as.data.frame(nacpChange[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),1])
  ngom <- as.data.frame(nacpChange[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),1])
  
  
  ### Tables of change NACP  1996-2016
  nacpChange <- data.frame('Subregion_Name'=nacpChange$A1_GNIS_Name,
                               'HUC_Number'=nacpChange$A2_Feat_Name,
                               'Swamp_Area_km2'= as.numeric(formatC(nacpChange$A4_Total_Pixels, digits = 0, format = 'f')),
                               'Area_Lost_km2'= as.numeric(formatC(nacpChange$A3_Change_Pixels, digits = 0, format = 'f')),
                               'Percent_Change'= as.numeric(formatC(nacpChange$A5_Change_Percent, digits = 1, format = 'f')))

  lossTotal <- sum(nacpChange[,4])
  acpLoss <- sum(nacpChange[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),4])
  ngomLoss <- sum(nacpChange[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),4])
  
  ### Tables of Gain NACP  1996-2016
  nacpGain <- data.frame('Subregion_Name'=mapGain$A1_GNIS_Name,
                         'HUC_Number'=mapGain$A2_Feat_Name,
                         'Swamp_Area_km2'= as.numeric(formatC(mapGain$A4_Total_Pixels, digits = 0, format = 'f')),
                         'Area_Gain_km2'= as.numeric(formatC(mapGain$A3_Change_Pixels, digits = 0, format = 'f')),
                         'Percent_Change'= as.numeric(formatC(mapGain$A5_Change_Percent, digits = 1, format = 'f')))
  gainTotal <- sum(nacpGain[,4])
  acpGain <- sum(nacpGain[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),4])
  ngomGain <- sum(nacpGain[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),4])
  
  nacpNet <- data.frame('Subregion_Name'=mapGain$A1_GNIS_Name,
                         'HUC_Number'=mapGain$A2_Feat_Name,
                         'Area_Lost_km2'= as.numeric(formatC(nacpChange[,4] - mapGain[,4], digits = 0, format = 'f')),
                        'Swamp_Area_km2'= as.numeric(formatC(nacpChange$Swamp_Area_km2, digits = 0, format = 'f')))
   
  ### Tables of change ACP  1996-2016
  acpChange <- data.frame('Subregion_Name'=acp,
                          'HUC_Number'=nacpNet[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),2],
                          'Swamp_Area_km2'= as.numeric(formatC(nacpNet[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),4], digits = 0, format = 'f')),
                          'Area_Lost_km2'= as.numeric(formatC(nacpNet[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),3], digits = 0, format = 'f')),
                          'Percent_Change'= as.numeric(formatC(nacpNet[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),5], digits = 1, format = 'f')))
  
  acpLost <- sum(nacpChange[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),4])
  acpGain <- sum(nacpGain[c(1,2,4,16,20,21,22,26,29,30,31,32,33,34,35,36,37,38,39,40,41,43,47,48,50),4])
  
  ### Tables of change nGOM  1996-2016
  ngomChange <- data.frame('Subregion_Name'=ngom,
                           'HUC_Number'=nacpNet[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),2],
                           'Swamp_Area_km2'= as.numeric(formatC(nacpNet[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),4], digits = 0, format = 'f')),
                           'Area_Lost_km2'= as.numeric(formatC(nacpNet[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),3], digits = 0, format = 'f')),
                           'Percent_Change'= as.numeric(formatC(nacpNet[c(3,5:15,17:19,23:25,27,28,42,44:46,49,51),5], digits = 1, format = 'f')))
  
  
  
  ### Getting subregions with 1996 area > 5000 km^2
  topChange <- nacpNet[nacpNet$Swamp_Area_km2 > 5000,]
  topChange$Percent_Change <-((topChange$Area_Lost_km2)/(topChange$Swamp_Area_km2))*100
  
  HUC_List <- write.csv(topChange, file = "HUC List.csv")
  
  ### Change Tables
  View(nacpChange)
  View(topChange)
  View(acpChange)
  View(ngomChange)
  
  ### Create a summary table from each region
  changeSummary <- data.frame("Region" = c("NACP", "14 Subregions", "ACP", "nGOM"),
                              'CFW Area' = c(sum(nacpNet$Swamp_Area_km2),
                                             sum(topChange$Swamp_Area_km2),
                                             sum(acpChange$Swamp_Area_km2),
                                             sum(ngomChange$Swamp_Area_km2)),
                              'CFW Change' = c(sum(nacpNet$Area_Lost_km2),
                                               sum(topChange$Area_Lost_km2),
                                               sum(acpChange$Area_Lost_km2),
                                               sum(ngomChange$Area_Lost_km2)),
                              'Percent Change' = c((sum(nacpNet$Area_Lost_km2)/sum(nacpNet$Swamp_Area_km2))*100,
                                                   (sum(topChange$Area_Lost_km2)/sum(topChange$Swamp_Area_km2))*100,
                                                   (sum(acpChange$Area_Lost_km2)/sum(acpChange$Swamp_Area_km2))*100,
                                                   (sum(ngomChange$Area_Lost_km2)/sum(ngomChange$Swamp_Area_km2))*100))
  
  View(changeSummary)
  
  
 
}

#######################
### Types of Change ###
#######################
{
  ### CFW Loss Pathways
  {
    swampFrequency<-t(swampFrequency)
    
    swampFrequency <- as.data.frame(swampFrequency)
    colnames(swampFrequency) <- "Area"
    rownames(swampFrequency) <- c("Palustrine Scrub/Shrub Wetland",
                                  'Estuarine Aquatic Bed',
                                  'Palustrine Emergent Wetland',
                                  'Estuarine Forested Wetland',
                                  'Estuarine Scrub/Shrub Wetland',
                                  'Estuarine Emergent Wetland',
                                  'Unconsolidated Shore',
                                  'Barren Land',
                                  'Open Water',
                                  'Palustrine Aquatic Bed',
                                  'Palustrine Forested Wetland',
                                  'Scrub/Shrub',
                                  'Evergreen Forest',
                                  'Mixed Forest',
                                  'Developed, High Intensity',
                                  'Developed, Medium Intensity',
                                  'Developed, Low Intensity',
                                  'Developed, Open Space',
                                  'Cultivated Crops',
                                  'Pasture/Hay',
                                  'Grassland/Herbaceous',
                                  'Deciduous Forest')
    
    swampFrequency$percent <- as.numeric(formatC((swampFrequency$Area/sum(swampFrequency[c(1:10,12:22),1]))*100, digits = 1, format = 'f'))
    
    ### Place NA for Palaustrine Forested Wetland
    swampFrequency[11,2] = NA
    
    View(swampFrequency)
    

  }
  
  ### CFW Gain Pathways
  {
    swampGain<-t(swampGain)
    
    swampGain <- as.data.frame(swampGain)
    colnames(swampGain) <- "Area"
    rownames(swampGain) <- c('Scrub/Shrub',
                             'Developed, Medium Intensity',
                             'Developed, High Intensity',
                             'Mixed  Forest',
                             'Evergreen Forest',
                             'Deciduous Forest',
                             'Grassland/Herbaceous',
                             'Pasture/Hay',
                             'Cultivated Crops',
                             'Developed, Open Space',
                             'Developed, Low Intensity',
                             'Palustrine Forested Wetland',
                             'Palustrine Scrub/Shrub Wetland',
                             'Estuarine Aquatic Bed',
                             'Palustrine Emergent Wetland',
                             'Estuarine Forested Wetland',
                             'Estuarine Scrub/Shrub Wetland',
                             'Estuarine Emergent Wetland',
                             'Unconsolidated Shore',
                             'Barren Land',
                             'Open Water',
                             'Palustrine Aquatic Bed')
    
    swampGain$percent <- as.numeric(formatC((swampGain$Area/sum(swampGain[c(1:11,13:22),1]))*100, digits = 1, format = 'f'))
    
    ### Place NA for Palaustrine Forested Wetland
    swampGain[12,2] = NA
    
    View(swampGain)
    
  }
  
}

################
### Alluvial ###
################
{
  riverColor <- c('red',
                  'purple',
                  'darkgreen',
                  'red',
                  'yellow',
                  'blue',
                  'gray',
                  'chocolate',
                  'purple',
                  'darkgreen')
  par(mar=c(3,3,3,1), mgp=c(1.75,0.5,0), cex = 1.25, family = "sans", font = 2, font.lab = 2)
  
  alluvial(swampAlluvial[,1:2], freq = swampAlluvial$Freq, alpha = 0.6, border = 'black', col = rev(riverColor))
}

#####################
### Fragmentation ### 
#####################
{
  meters <- 27.4585743132134
  pixels <- 2
  
  ### Unit conversions
  m2km <- 1e-3 # meters to kilometers
  sqkm <- 1e-6 # m^2 to km^2
  
  ### create threshold for filtering 
  areaCut <- (meters^2)*pixels
  
  ### Both Neuse-Pamlico and Chowan-Roanoke Subregions
  {
    #Selecting the area and perimeter values for each year
    geo1996 <- geo1996[,c(3,4)]
    geo2017 <- geo2017[,c(3,4)]
    
    #filtering area to only allow patches of â¥ 3 pixels
    geo1996 <- geo1996[geo1996$AREA > areaCut,] 
    geo2017 <- geo2017[geo2017$AREA > areaCut,] 
  
    #converting values to metric
    geo1996$AREA <-geo1996$AREA*sqkm
    geo1996$PERIMETER <- geo1996$PERIMETER*m2km
    
    geo2017$AREA <-geo2017$AREA*sqkm
    geo2017$PERIMETER <- geo2017$PERIMETER*m2km
    
    #generate the perimeter to area metric (P:A)
    geo1996$ratio <- geo1996$PERIMETER/geo1996$AREA
    geo2017$ratio <- geo2017$PERIMETER/geo2017$AREA
    
  
    #Perimeter
    mean(geo1996$PERIMETER)
    sd(geo1996$PERIMETER)
    mean(geo2017$PERIMETER)
    sd(geo2017$PERIMETER)
    
    median(geo1996$PERIMETER)
    mad(geo1996$PERIMETER)
    median(geo2017$PERIMETER)
    mad(geo2017$PERIMETER)
    
    max(geo1996$PERIMETER)
    max(geo2017$PERIMETER)
    
    #Area
    mean(geo1996$AREA)
    sd(geo1996$AREA)
    mean(geo2017$AREA)
    sd(geo2017$AREA)
    
    median(geo1996$AREA)
    mad(geo1996$AREA)
    median(geo2017$AREA)
    mad(geo2017$AREA)
    
    max(geo1996$AREA)
    max(geo2017$AREA)
    
    #Ratio
    mean(geo1996$ratio)
    sd(geo1996$ratio)
    mean(geo2017$ratio)
    sd(geo2017$ratio)
    
    
    median(geo1996$ratio)
    mad(geo1996$ratio)
    median(geo2017$ratio)
    mad(geo2017$ratio)
    
    max(geo1996$ratio)
    max(geo2017$ratio)
    
    bigPatch96 <- geo1996[geo1996$AREA >25,]
    bigPatch17 <- geo2017[geo2017$AREA >25,]
    
    sum(geo1996$AREA)/sum(bigPatch96$AREA)
    sum(geo2017$AREA)/sum(bigPatch17$AREA)
    
    sum(bigPatch96$AREA)
    sum(bigPatch17$AREA)
    
    sum(bigPatch96$AREA)/sum(geo1996$AREA)
    sum(bigPatch17$AREA)/sum(geo2017$AREA)
    
  
    
    ###Stat
    #MWU
    wilcox.test(geo1996$AREA, geo2017$AREA)
    wilcox.test(geo1996$PERIMETER, geo2017$PERIMETER)
    wilcox.test(geo1996$ratio, geo2017$ratio)
    
    #ks.test
    ks.test(geo1996$AREA, geo2017$AREA)
    ks.test(geo1996$PERIMETER, geo2017$PERIMETER)
    ks.test(geo1996$ratio, geo2017$ratio)
    
  
    #Plot
    {
      par(mar=c(3,3,3,3), mgp=c(1.75,0.5,0), cex = 1.25, family = "sans", font = 2, font.lab = 2)
      
      plot(density(geo2017$ratio), xlim=c(0,150), type = "n", font = 2,
           xlab='Perimeter to Area Ratio', main= 'Fragmentation in the Neuse-Pamlico and Chowan-Roanoke Subregions')
      par(new=T)
      lines(density(geo1996$ratio), col="red", lwd=2)
      lines(density(geo2017$ratio), col="blue", lwd=2)
      par(new=T)
      hist(geo2017$ratio, breaks = 100, xlim = c(0,150), ylim = c(0,2250), col = 'blue', xaxt ='n', yaxt = 'n', ylab = NA, xlab = NA, main = NA)
      par(new=T)
      hist(geo1996$ratio, breaks = 100, xlim = c(0,150), ylim = c(0,2250), col = 'red', xaxt ='n', yaxt = 'n', ylab = NA, xlab = NA, main = NA)
      mtext(seq(0,2250,500), side = 4, line = 0, at = seq(0,2250,500), cex = 1.5)
      mtext('# of Patches', side = 4, line = 1, at = 1125, cex = 1.5)
      abline(v=c(median(geo1996$ratio),median(geo2017$ratio)), lty=2, lwd=3, col= c('red','blue'))
      legend('topleft', bty="n", legend = c('1996','2017','KDE: KS p > 0.05','Histogram','Median: MWU p > 0.05'),
            cex=1, lty = c(NA,NA,1,NA,2), pch = c(15,15,NA,15,NA), col = c('red','blue','black','black','black'), lwd = c(NA,NA,3,NA,3))
      }
    
  }
  
  ### Palmetto-Peartree Preserve
  {
    #Selecting the area and perimeter values for each year
    palmPear1996 <- palmPear1996[,c(3,4)]
    palmPear2017 <- palmPear2017[,c(3,4)]
    
    #filtering area to only allow patches of â¥ 3 pixels
    palmPear1996 <- palmPear1996[palmPear1996$AREA > areaCut,] 
    palmPear2017 <- palmPear2017[palmPear2017$AREA > areaCut,] 
    
    #converting values to metric
    palmPear1996$AREA <-palmPear1996$AREA*sqkm
    palmPear1996$PERIMETER <- palmPear1996$PERIMETER*m2km
    
    palmPear2017$AREA <-palmPear2017$AREA*sqkm
    palmPear2017$PERIMETER <- palmPear2017$PERIMETER*m2km
    
    #generate the perimeter to area metric (P:A)
    palmPear1996$ratio <- palmPear1996$PERIMETER/palmPear1996$AREA
    palmPear2017$ratio <- palmPear2017$PERIMETER/palmPear2017$AREA
    
    #Perimeter
    mean(palmPear1996$PERIMETER)
    sd(palmPear1996$PERIMETER)
    mean(palmPear2017$PERIMETER)
    sd(palmPear2017$PERIMETER)
    
    median(palmPear1996$PERIMETER)
    mad(palmPear1996$PERIMETER)
    median(palmPear2017$PERIMETER)
    mad(palmPear2017$PERIMETER)
    
    max(palmPear1996$PERIMETER)
    max(palmPear2017$PERIMETER)
    
    #Area
    mean(palmPear1996$AREA)
    sd(palmPear1996$AREA)
    mean(palmPear2017$AREA)
    sd(palmPear2017$AREA)
    
    median(palmPear1996$AREA)
    mad(palmPear1996$AREA)
    median(palmPear2017$AREA)
    mad(palmPear2017$AREA)
    
    max(palmPear1996$AREA)
    max(palmPear2017$AREA)
    
    #Ratio
    mean(palmPear1996$ratio)
    sd(palmPear1996$ratio)
    mean(palmPear2017$ratio)
    sd(palmPear2017$ratio)
    
    median(palmPear1996$ratio)
    mad(palmPear1996$ratio)
    median(palmPear2017$ratio)
    mad(palmPear2017$ratio)
    
    max(palmPear1996$ratio)
    max(palmPear2017$ratio)
    
    
    ###Stat
    #MWU
    wilcox.test(palmPear1996$AREA, palmPear2017$AREA)
    wilcox.test(palmPear1996$PERIMETER, palmPear2017$PERIMETER)
    wilcox.test(palmPear1996$ratio, palmPear2017$ratio)
    
    #ks.test
    ks.test(palmPear1996$AREA, palmPear2017$AREA)
    ks.test(palmPear1996$PERIMETER, palmPear2017$PERIMETER)
    ks.test(palmPear1996$ratio, palmPear2017$ratio)
    
    #Plot
    
    {
      par(mar=c(3,3,3,3), mgp=c(1.75,0.5,0), cex = 1.25, family = "sans", font = 2, font.lab = 2)
      
      plot(density(palmPear2017$ratio), xlim=c(0,150), type = "n", xlab='Perimeter to Area Ratio', font = 2,
           main= 'Fragmentation in Palmetto-Peartree Preserve')
      par(new=T)
      lines(density(palmPear1996$ratio), col="red", lwd=2)
      lines(density(palmPear2017$ratio), col="blue", lwd=2)
      par(new=T)
      hist(palmPear2017$ratio, breaks = 20, xlim = c(0,150), ylim = c(0,50), col = 'blue', xaxt ='n', yaxt = 'n', ylab = NA, xlab = NA, main = NA)
      par(new=T)
      hist(palmPear1996$ratio, breaks = 20, xlim = c(0,150), ylim = c(0,50), col = 'red', xaxt ='n', yaxt = 'n', ylab = NA, xlab = NA, main = NA)
      mtext(seq(0,50,10), side = 4, line = 0, at = seq(0,50,10), cex = 1.5)
      mtext('# of Patches', side = 4, line = 1, at = 25, cex = 1.5)
      abline(v=c(median(palmPear1996$ratio),median(palmPear2017$ratio)), lty=2, lwd=3, col= c('red','blue'))
      legend('topleft', bty="n", legend = c('1996','2017','KDE: KS p > 0.05','Histogram','Median: MWU p > 0.05'),
             cex=1, lty = c(NA,NA,1,NA,2), pch = c(15,15,NA,15,NA), col = c('red','blue','black','black','black'), lwd = c(NA,NA,3,NA,3))
      
    }
    
  
  }
}

################
### Modeling ###
################
{
  logCor <- log(newCor[,c(3:8)])
  
  ### Full Model
  swampModel <- glm(AreaLost ~ Drainage.Density + Storms + SLR + elevation + slope, data = logCor)
  summary(swampModel)
  swampModelSum <- summary(swampModel)
  rsq(swampModel, adj=T)
  
  stepSwamp<-step(swampModel, direction = "both")
  
  ### Reduced Model
  swampModelBest <- glm(AreaLost ~ Drainage.Density + Storms + SLR + slope, data = logCor)
  summary(swampModelBest)
  swampBestSum <- summary(swampModelBest)
  rsq(swampModelBest, adj=T)
  
  #Overall Model Performance
  rmse(logCor$AreaLost, fitted(swampModel)) 
  rmse(logCor$AreaLost, fitted(swampModelBest)) 
  
  AICc(swampModel)
  AICc(swampModelBest)
  
  ###Plot Observations and Model
  {
  par(mar=c(3,3,3,1), mgp=c(1.75,0.5,0), cex = 1.25, family = "sans", font = 2, font.lab = 2)
  plot(logCor$AreaLost, fitted(swampModel), 
       xlab = "Observed Forest Area Lost (log transformed)", 
       ylab = "Predicted Forest Area Lost (log transformed)",
       main = "Model of Coastal Forested Wetland Loss in the NACP between 1996-2016",
       xlim=c(5,8),
       ylim=c(5,8),
       pch=16, col= 'blue', cex = 3, font= 2)
  par(new=T)
  plot(logCor$AreaLost, fitted(swampModelBest), xlab = NA, ylab = NA, xaxt = 'n', yaxt = 'n', 
       xlim=c(5,8),
       ylim=c(5,8),
       pch=16, col= 'red', cex = 3,font= 2)
  abline(a = 0, b = 1, lwd=3, lty=2)
  legend('topleft', bty='n', legend = c('Full Model, R^2 = 0.78', 'Reduced Model, R^2 = 0.80', '1:1 Line'), 
         col= c('blue', 'red', 'black'), pch=c(16,16,NA), lty=c(NA,NA,2), lwd=c(NA,NA,3), cex = 1.5)
  text(logCor$AreaLost, fitted(swampModelBest), font = 2)
  }

}
