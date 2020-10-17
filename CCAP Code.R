### Install Packages
install.packages(rjson)
install.packages(rsq)

### Load Package
library(rjson)
library(rsq)

###########################################################################
### IMPORT DICTIONARIES FROM GEE. THEY SHOULD BE SAVED AS A .json FILE. ###
###########################################################################
{
  ### Change from 1996 to 2016 for different regions
  {
    nacpChange <- as.data.frame(fromJSON(file = 'nacpChange1996_2016.json'))
    acpChange <- as.data.frame(fromJSON(file = 'acpChange1996_2016.json'))
    ngomChange <- as.data.frame(fromJSON(file = 'ngomChange1996_2016.json'))
  }
  
  ### Area of CFW for each Year
  {
    swampAreaDict <- as.data.frame((fromJSON(file = 'swampDict2016.json')))
  }
  
  ### Frequency Histograms
  {
    ### The 0.0009 converts the count of pixels to km^2
    swampFrequency <- as.data.frame(fromJSON(file = 'swampFrequency.json'))*.0009
    swampGain <- as.data.frame(fromJSON(file = 'swampGain.json'))*.0009
    waterFrequency <- as.data.frame(fromJSON(file = 'waterFrequency.json'))*.0009
  }
}
  
#####################
### Create tables ###
#####################
{
  ### Tables of change NACP  1996-2016
  nacpChange <- data.frame('Subregion_Name'=nacpChange$A1_GNIS_Name,
                               'HUC_Number'=nacpChange$A2_Feat_Name,
                               'Swamp_Area_km2'= as.numeric(formatC(nacpChange$A4_Total_Pixels, digits = 0, format = 'f')),
                               'Area_Lost_km2'= as.numeric(formatC(nacpChange$A3_Change_Pixels, digits = 0, format = 'f')),
                               'Percent_Change'= as.numeric(formatC(nacpChange$A5_Change_Percent, digits = 1, format = 'f')))
  
  ### Tables of change ACP  1996-2016
  acpChange <- data.frame('Subregion_Name'=acpChange$A1_GNIS_Name,
                           'HUC_Number'=acpChange$A2_Feat_Name,
                           'Swamp_Area_km2'= as.numeric(formatC(acpChange$A4_Total_Pixels, digits = 0, format = 'f')),
                           'Area_Lost_km2'= as.numeric(formatC(acpChange$A3_Change_Pixels, digits = 0, format = 'f')),
                           'Percent_Change'= as.numeric(formatC(acpChange$A5_Change_Percent, digits = 1, format = 'f')))
  
  ### Tables of change nGOM  1996-2016
  ngomChange <- data.frame('Subregion_Name'=ngomChange$A1_GNIS_Name,
                           'HUC_Number'=ngomChange$A2_Feat_Name,
                           'Swamp_Area_km2'= as.numeric(formatC(ngomChange$A4_Total_Pixels, digits = 0, format = 'f')),
                           'Area_Lost_km2'= as.numeric(formatC(ngomChange$A3_Change_Pixels, digits = 0, format = 'f')),
                           'Percent_Change'= as.numeric(formatC(ngomChange$A5_Change_Percent, digits = 1, format = 'f')))
  
  ### Just the top 10% based on total area lost
  #top10Change <- nacpChange[nacpChange$Percent_Change > 9.99,]  
  top10Change <- top10Change[top10Change$Swamp_Area_km2 > 5000,]

  HUC_List <- write.csv(top10Change, file = "HUC List.csv")
  
  ### Top 10 table based on loss
  View(nacpChange)
  View(top10Change)
  View(acpChange)
  View(ngomChange)
  
  ### Create a summary table from each region
  changeSummary <- data.frame("Region" = c("NACP", "Top 10", "ACP", "nGOM"),
                              'CFW Area' = c(sum(nacpChange$Swamp_Area_km2),
                                             sum(top10Change$Swamp_Area_km2),
                                             sum(acpChange$Swamp_Area_km2),
                                             sum(ngomChange$Swamp_Area_km2)),
                              'CFW Change' = c(sum(nacpChange$Area_Lost_km2),
                                               sum(top10Change$Area_Lost_km2),
                                               sum(acpChange$Area_Lost_km2),
                                               sum(ngomChange$Area_Lost_km2)),
                              'Percent Change' = c((sum(nacpChange$Area_Lost_km2)/sum(nacpChange$Swamp_Area_km2))*100,
                                                   (sum(top10Change$Area_Lost_km2)/sum(top10Change$Swamp_Area_km2))*100,
                                                   (sum(acpChange$Area_Lost_km2)/sum(acpChange$Swamp_Area_km2))*100,
                                                   (sum(ngomChange$Area_Lost_km2)/sum(ngomChange$Swamp_Area_km2))*100))
  
  View(changeSummary)
}

#########################
### Create Line Graph ###
#########################
{
  ###Table of area of CFW each year
  swampAreaTable <- data.frame('Subregion_Name'=swampAreaDict$A1_GNIS_Name,
                               'HUC_Number'=swampAreaDict$A2_HUC_Name,
                               'Area96'= as.numeric(formatC(swampAreaDict$A3_Area96, digits = 0, format = 'f')),
                               'Area01'= as.numeric(formatC(swampAreaDict$A4_Area01, digits = 0, format = 'f')),
                               'Area06'= as.numeric(formatC(swampAreaDict$A5_Area06, digits = 0, format = 'f')),
                               'Area10'= as.numeric(formatC(swampAreaDict$A6_Area10, digits = 0, format = 'f')),
                               'Area16'= as.numeric(formatC(swampAreaDict$A7_Area16, digits = 0, format = 'f')))
  
  top10AreaLost <- swampAreaTable[c(41,38,42,36,31,35,30,43,49,46,44),]
  View(top10AreaLost)

  acpArea <- swampAreaTable$HUC_Number >= 0309
  
  ###Line plot of the Top 10
  {
    png('Figure 2.png', pointsize = 10, width = 4750, height = 6000, res = 600)
  
  
    par(mar=c(3,3,3,1), mgp=c(1.75,0.5,0), cex=1.5)
    
    years <- c(1996,2001,2006,2010,2016)
    plot(years, top10AreaLost[1,c(3:7)], type='b', lwd=3, pch='1', ylim=c(min(top10AreaLost[,c(3:7)]),max(top10AreaLost[,c(3:7)])),
         xlab='Year', ylab='Area (km^2)', main='Coastal Forested Wetland Areal Coverage in HUC Subregions', col='darkblue')
    lines(years, top10AreaLost[2,c(3:7)], type='b', lwd=3, pch='2', col='darkblue')
    lines(years, top10AreaLost[3,c(3:7)], type='b', lwd=3, pch='3', col='red')
    lines(years, top10AreaLost[4,c(3:7)], type='b', lwd=3, pch='4', col='darkblue')
    lines(years, top10AreaLost[5,c(3:7)], type='b', lwd=3, pch='5', col='darkblue')
    lines(years, top10AreaLost[6,c(3:7)], type='b', lwd=3, pch='6', col='darkblue')
    lines(years, top10AreaLost[7,c(3:7)], type='b', lwd=3, pch='7', col='darkblue')
    lines(years, top10AreaLost[8,c(3:7)], type='b', lwd=3, pch='8', col='red')
    lines(years, top10AreaLost[9,c(3:7)], type='b', lwd=3, pch='9', col='red')
    lines(years, top10AreaLost[10,c(3:7)], type='b', lwd=3, pch='', col='red')
    lines(years, top10AreaLost[11,c(3:7)], type='b', lwd=3, pch='', col='red')
    #abline(v=c(1999,2005), col=c('darkblue', 'red'), lty=2, lwd=3)
    legend('topright', bty='n', legend = c('ACP', 'nGOM'), 
           pch = 15, col = c('darkblue','red'))
    text(years, top10AreaLost[10,c(3:7)], '10', col='darkblue')
    text(years, top10AreaLost[11,c(3:7)], '11', col='red')
    #text(2001, 10500, 'Hurricane George', col='darkblue')
    #text(2003, 10000, 'Hurricane Katrina', col='red')
  
  
    dev.off()
  }
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
    swampFrequency[11,2] = NA
    
    sum(swampFrequency[c(1:10,12:22),1])
    
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
    swampGain[12,2] = NA
    
    View(swampGain)
    
    sum(swampGain[c(1:11,13:22),1])
  }
  
  ### Land Loss Pathways
  {
    waterFrequency<-t(waterFrequency)
    
    waterFrequency <- as.data.frame(waterFrequency)
    colnames(waterFrequency) <- "Area"
    rownames(waterFrequency) <- c('Barren Land',
                                  'Mixed  Forest',
                                  'Evergreen Forest',
                                  'Deciduous Forest',
                                  'Grassland/Herbaceous',
                                  'Pasture/Hay',
                                  'Cultivated Crops',
                                  'Developed, Open Space',
                                  'Developed, Low Intensity',
                                  'Developed, Medium Intensity',
                                  'Developed, High Intensity',
                                  'Unconsolidated Shore',
                                  'Estuarine Emergent Wetland',
                                  'Estuarine Scrub/Shrub Wetland',
                                  'Estuarine Forested Wetland',
                                  'Palustrine Emergent Wetland',
                                  'Palustrine Scrub/Shrub Wetland',
                                  'Palustrine Forested Wetland',
                                  'Scrub/Shrub',
                                  'Open Water',
                                  'Palustrine Aquatic Bed',
                                  'Estuarine Aquatic Bed')
    
    waterFrequency$percent <- as.numeric(formatC((waterFrequency$Area/sum(waterFrequency[c(1:19,21:22),1]))*100, digits = 1, format = 'f'))
    waterFrequency[20,2] = NA
    
    sum(waterFrequency[c(1:19,21:22),1])
    
    View(waterFrequency)
    }
}



###################
### Correlation ###
###################

swampCor <- read.csv('swampCorrelation.csv', row= 1)
swampCor <- swampCor[,-1]
terrainStuff <- as.data.frame(fromJSON(file = 'terrain.json'))
swampCor$elevation <- terrainStuff[c(30,31,35,36,38,41,42,43,44,46,49),3]
swampCor$slope <- terrainStuff[c(30,31,35,36,38,41,42,43,44,46,49),4]
swampCor$percent <- nacpChange[c(30,31,35,36,38,41,42,43,44,46,49),5]

swampCor <- log(swampCor)

swampCor$color <- c(rep('darkblue',6), rep('red',5))

#swampCor <- swampCor[-4,]

swampModel <- glm(swampCor$AreaLost ~ swampCor$SLR + swampCor$Drainage.Density + swampCor$Storms + swampCor$elevation + swampCor$slope)

summary(swampModel)
swampModelSum <- summary(swampModel)
rsq(swampModel, adj=T)

stepSwamp<-step(swampModel, direction = "both")

swampModelBest <- glm(swampCor$AreaLost ~ swampCor$SLR + swampCor$Storms + swampCor$Drainage.Density)

stepSwamp2<-step(swampModelBest, direction = "both")

summary(swampModelBest)

rsq(stepSwamp2, adj=T)

{
par(mar=c(3,3,3,1), mgp=c(1.75,0.5,0), mfrow= c(3,2))
plot(swampCor$Storms,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = '# of Storms',  main = 'Storms ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
stormLost <- lm(swampCor$AreaLost ~ swampCor$Storms)
abline(stormLost, lty = 2, lwd = 3)
sumStormLost <- summary(stormLost)
legend('topleft', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$Storms,swampCor$AreaLost), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumStormLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumStormLost[["coefficients"]][2,4], format = 'f', digits = 3))))

plot(swampCor$SLR,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = 'Rate of SLR (cm/yr)',  main = 'SLR ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
slrLost <- lm(swampCor$AreaLost ~ swampCor$SLR)
abline(slrLost, lty = 2, lwd = 3)
sumSLRLost <- summary(slrLost)
legend('topright', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$SLR,swampCor$AreaLost, use = 'complete.obs'), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumSLRLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumSLRLost[["coefficients"]][2,4], format = 'f', digits = 3))))

plot(swampCor$Drainage.Density,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = 'Drainage Density (km/km)',  main = 'Drainage Density ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
ddLost <- lm(swampCor$AreaLost ~ swampCor$Drainage.Density)
abline(ddLost, lty = 2, lwd = 3)
sumDDLost <- summary(ddLost)
legend('topright', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$Drainage.Density,swampCor$AreaLost, use = 'complete.obs'), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumDDLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumDDLost[["coefficients"]][2,4], format = 'f', digits = 3))))


plot(swampCor$elevation,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = 'Subregion Average Elevation (m)',  main = 'Average Elevation ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
elevationLost <- lm(swampCor$AreaLost ~ swampCor$elevation)
abline(elevationLost, lty = 2, lwd = 3)
sumElevationLost <- summary(elevationLost)
legend('topleft', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$elevation,swampCor$AreaLost, use = 'complete.obs'), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumElevationLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumElevationLost[["coefficients"]][2,4], format = 'f', digits = 3))))

plot(swampCor$slope,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = 'Subregion Average Slope',  main = 'Average Slope ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
slopeLost <- lm(swampCor$AreaLost ~ swampCor$slope)
abline(slopeLost, lty = 2, lwd = 3)
sumSlopeLost <- summary(slopeLost)
legend('topleft', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$slope,swampCor$AreaLost, use = 'complete.obs'), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumSlopeLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumSlopeLost[["coefficients"]][2,4], format = 'f', digits = 3))))

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', main = 'Area Lost ~ SLR + Drainage + Storms
     + Elevation + Slope', cex = 2)
legend('center', bty = 'n', legend = c(paste('coef. of determination=', formatC(rsq(swampModel, adj=T), format = 'f', digits = 2)),
                                       '',
                                       'p-value',
                                       paste('Intercept=', formatC(swampModelSum[["coefficients"]][1,4], format = 'f', digits = 3)),
                                       paste('SLR=', formatC(swampModelSum[["coefficients"]][2,4], format = 'f', digits = 3)),
                                       paste('Drainage=', formatC(swampModelSum[["coefficients"]][3,4], format = 'f', digits = 3)),
                                       paste('Storm=', formatC(swampModelSum[["coefficients"]][4,4], format = 'f', digits = 3)),
                                       #paste('1996 Area=', formatC(swampModelSum[["coefficients"]][5,4], format = 'f', digits = 3)),
                                       paste('Elevation=', formatC(swampModelSum[["coefficients"]][5,4], format = 'f', digits = 3)),
                                       paste('Slope=', formatC(swampModelSum[["coefficients"]][6,4], format = 'f', digits = 3)),
                                       '',
                                       '(*) denotes p-value < 0.05'), cex=1)

legend('right', bty = 'n', legend = c('ACP', 'nGOM'), pch = 16, col = c('darkblue', 'red'), cex= 1.5)
}


plot(swampCor$Area1996,swampCor$AreaLost, ylab = 'Area of CFW Lost (km^2)', xlab = 'CFW Area 1996 (km^2)',  main = '1996 Area ~ Area Lost', 
     pch = 16, col = swampCor$color, cex= 1.5)
AreaLost <- lm(swampCor$AreaLost ~ swampCor$Area1996)
abline(AreaLost, lty = 2, lwd = 3)
sumAreaLost <- summary(AreaLost)
legend('topleft', bty= 'n', legend = c(paste('correlation r=', formatC(cor(swampCor$Area1996,swampCor$AreaLost, use = 'complete.obs'), format = 'f', digits = 2)),
                                       paste('trend line r^2=', formatC(sumAreaLost[["adj.r.squared"]], format = 'f', digits = 2)),
                                       paste('trend line p-value=', formatC(sumAreaLost[["coefficients"]][2,4], format = 'f', digits = 3),'*')))
