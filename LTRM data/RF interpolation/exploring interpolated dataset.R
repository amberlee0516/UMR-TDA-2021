#code by Danelle Larson 07/15/2021; dmlarson@usgs.gov
#exploring the interpolated LTRM water dataset 
#provided by Amber Lee on 07/15/2021
#TN, TP and VEL variables were interpolated using random forests; other variables have NAs lingering
#note some variable level names were changed for ease of reading (e.g., FLDNUM and STRATUM were changed from numeric to text)



#SETUP & LIBRARY-----------------
setwd( "C:\\Users\\dmlarson\\OneDrive - DOI\\SAV regimes framework\\TDA approach\\interpolation of ltrm water data\\interpolated data using random forests\\")

library(tidyverse)
library(reshape2)
library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


## LOAD ORIGINAL DATASETS -----
#LOAD CLEANED VEG DATASET
#extensive cleaning and manipulation done using the "JSDM full dataset.R" file in the hmsc folder
#includes major pools, strata, and sites (including 15,000 with no veg detected)
all_WQ<-read.csv("C:\\Users\\dmlarson\\OneDrive - DOI\\SAV regimes framework\\TDA approach\\interpolation of ltrm water data\\interpolated data using random forests\\water_full.csv") #years through 2019 from Larson;downloaded off LTRM website Nov 2020; created using r script "JSDM full dataset" in the hmsc folder
str(all_WQ)

length(unique(all_WQ$SHEETBAR)) #all barcodes are unique-- good



sum(duplicated(all_WQ[,c("LATITUDE","LONGITUDE")]))
duplicateUTM<-which(duplicated(all_WQ[,c("LATITUDE","LONGITUDE")])) #half of sites are duplicated lat/long sites. Not sure why. Ask Doug how this information was calculated. Could check against the EASTING/NORTHING variables in original dataset

summary(all_WQ$SEASON) #4 seasons

table(all_WQ$FLDNUM,all_WQ$STRATUM) #looks accurate counts and strata by pool

table(all_WQ$FLDNUM,all_WQ$SEASON) #4 seasons) #consistent spread across seasons; winter has slightly fewer due to accessibility issues from ice

table(all_WQ$FLDNUM,all_WQ$YEAR) #we would expect 600 sites per year per pool with perfect sampling; this looks reasonable


#variables----------
attach(all_WQ)

hist(TEMP)
summary(TEMP)
boxplot(TEMP)

hist(WDP)

hist(SECCHI) #larger range and normal distribution compared to TURB
summary(SECCHI) #433 NA missing data

hist(TURB)
summary(TURB) 
boxplot(TURB)

hist(SS)
boxplot(SS)
summary(SS) 

hist(DO)

hist(VEL)
summary(VEL) #no NA, all missing values were interpolated with random forests


hist(PH)

hist(TN)
summary(TN)
boxplot(TN) #the three extreme outliers were excluded from interpolation, but retained in the entire dataframe for subsequent analyses (like TDA)

hist(TP)
summary(TP) #going to reduce dataframe down to like 4,000 obs unless I interpolate

hist(CHLcal)
summary(CHLcal)
boxplot(CHLcal)

hist(WDP)





#misc
#variable only dataframe 
WQparameters<-select(all_WQ,TN,TP,SS,SECCHI,TURB,TEMP,DO,VEL,CHLcal,WDP,COND)

# create raw correlation matrix
library(corrplot)
corrplot(cor(WQparameters), type = "upper", method = "ellipse", tl.cex = 0.9)

#dimension reduction to visualize (e.g.PCA method)
library(FactoMineR)
library(factoextra)
res.pca <- PCA(WQparameters,  graph = FALSE)
res.pca
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("water quality relationships in the Upper Mississippi River") + labs(fill = "contribution")



ggplot(all_WQ, aes(YEAR, CHLcal)) +
  #geom_point()+ #adds scatterplot
  scale_x_continuous(name="Year",breaks=seq(1993,2020,by=1))+ #controls x-axis title and tick intervals
  stat_smooth(method = "loess",se=T,linetype="solid",inherit.aes=T)+
  #facet_grid(FLDNUM~.,scales="free_y")+
  facet_grid(FLDNUM~.)+
  theme_bw()
