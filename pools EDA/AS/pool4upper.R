#################################
# Author: Alaina Stockdill
# Project: Exploring the upper pool 4 
#################################

# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# Look at the continuous variables
setwd("/Users/alainastockdill/UMR-TDA-2021/pools EDA/AS")

# Upload the data
veg <- read.csv(file = "../2019 pool data/ltrm_vegsrs_data_lat_long.csv")
water <- read.csv(file = "../2019 pool data/ltrm_water_data_lat_long.csv")

# Split the veg and water data into pool 4
veg4 <- veg %>% 
  filter(POOL == "04")

water4 <- water %>%
  filter(FLDNUM == "1")

# Get just the year and put in new coloumn
water4$DATE <- as.Date(water4$DATE, "%m/%d/%Y")
water4$year <- year(water4$DATE)

# Get the seasons
water4$QUARTER <- quarter(water4$DATE, with_year = FALSE, fiscal_start = 1)

# Split into the upper data
veg4U <- veg4 %>%
  filter(MSTRATUM == "BWC-U")

water4U <- water4 %>% 
  filter(LONGITUDE > -92.035)

# Create a set that is just the continuous variables
continuous <- c('TN','TP','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI')
water4_cont <- water4[continuous]

# Show the split of upper pool 4 on the entire pool data
ggplot(water4, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point() +
geom_hline(yintercept = -92.035, color = "red")


# Show the split of upper pool 4 zoomed in
water4U_test = water4 %>% 
  filter(LONGITUDE > -92.05)

ggplot(data = water4U_test, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point() +
  geom_hline(yintercept = -92.035, color = "red")


# Different upper pool 4 coloring
  ggplot(water4U, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point(mapping = aes(color = STRATUM))


# BOX PLOTS
water4U %>%
  filter(TURB < 100) %>%
ggplot(, mapping = aes(y = TURB, x = year, group = year)) + 
  geom_boxplot()


water4U %>%
  filter( SS < 300) %>%
  ggplot(, mapping = aes(y = SS, x = year, group = year)) + 
  geom_boxplot()

  ggplot(water4U, mapping = aes(y = SS, x = year, group = year)) + 
  geom_boxplot()


ggplot(data = wzter4, mapping = aes(x = SECCI)) +
  geom_histogram()


ggplot(data = water4, mapping = aes(x = SECCHI, y = TURB )) + 
  geom_point(mapping = aes(color = TP), alpha = .1)
 
unique(water4$TURB)



ggplot(data = water4) + 
  geom_boxplot(mapping = aes(x = TURB))


# TN is least change - little variability
# TEMP inconsistent
# TO variable between time of day


unique(water4U$STRATUM)

ggplot(data = water4U, mapping = aes(x = year, y = TURB)) + 
  geom_point() +
  facet_wrap(~STRATUM, nrow = 3, scale = free)


### AMBER'S CODE
continuous <- c('TN','TP','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI')
# BOXPLOTS
plotter_box_by_year <- function(var_str, data, facet_bool){
  
  # facet_bool gives if you should facet by STRATUM type
  
  title <- paste("boxplot_by_year", "4U", var_str, sep = "_")
  
  if (facet_bool){title <- paste(title, "_facet")}
  
  title <- paste(title, "png", sep = ".")
  
  if (facet_bool){
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = year, y = !!sym(var_str), group = year)) +
      geom_boxplot() + 
      facet_wrap(~ QUARTER)
  } else {
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = year, y = !!sym(var_str), group = year)) +
      geom_boxplot()
  }
  
  ggsave(title)
  
}

sapply(continuous, plotter_box_by_year, water4U, T )

plotter_box_by_year("TURB")


# Scatter plots
plotter_point_by_year <- function(var_str, data, facet_bool){
  
  # facet_bool gives if you should facet by STRATUM type
  
  title <- paste("scatterplot_by_year", "4U", var_str, sep = "_")
  
  if (facet_bool){title <- paste(title, "_facet")}
  
  title <- paste(title, "png", sep = ".")
  
  if (facet_bool){
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = year, y = !!sym(var_str), group = year)) +
      geom_point() + 
      facet_wrap(~ QUARTER)
  } else {
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = year, y = !!sym(var_str), group = year)) +
      geom_point()
  }
  
  ggsave(title)
  
}
sapply(continuous, plotter_point_by_year, water4U, T)


# Using multicollinearity
pairs(x = water4U[,continuous], pch = 16)














  