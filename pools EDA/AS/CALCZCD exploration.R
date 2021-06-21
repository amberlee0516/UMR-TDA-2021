# Duplicate columns
# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(MASS)

# Look at the continuous variables
setwd("/Users/alainastockdill/UMR-TDA-2021/pools EDA/AS")

# Water data
water <- read.csv(file = "../pool data/ltrm_water_data_lat_long.csv")

# Filter by the calczcd

water_surf <- water %>% filter(CALCZCD == 'SF')

water_surf_missing <- water %>% filter(CALCZCD == '')


# Find number of missing values - in total data set
missing <- table(c(sum(is.na(main$TP)), sum(is.na(side$TP)), sum(is.na(backwater$TP)),
                   sum(is.na(main$CHLcal)), sum(is.na(side$CHLcal)), sum(is.na(backwater$CHLcal)),
                   sum(is.na(main$SS)), sum(is.na(side$SS)), sum(is.na(backwater$SS)),
                   sum(is.na(main$TURB)), sum(is.na(side$TURB)), sum(is.na(backwater$TURB))),
                 ncol = 3, byrow = TRUE)

sum(is.na(water4U$TP))
sum(is.na(water4U$CHLcal))
sum(is.na(water4U$SS))
sum(is.na(water4U$TURB))
length(water4U$TP)

sum(is.na(backwater$TP))
sum(is.na(backwater$CHLcal))
sum(is.na(backwater$SS))
sum(is.na(backwater$TURB))
length(main$TP)

# Look at missing values by stratum - looks only at the continuous
water4U_cont = water4U[continuous]

water4U_surf <- water4U_cont %>% filter(CALCZCD == "SF")

length(unique(water4U_surf$SHEETBAR))

sum(is.na(water4U_surf$CHLcal))
sum(!is.na(water4U_surf$CHLcal))
