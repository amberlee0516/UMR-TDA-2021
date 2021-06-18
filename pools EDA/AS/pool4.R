#################################
# Author: Alaina Stockdill
# Project: Exploring the upper pool 4 
#################################

# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(MASS)

# Set the working directory to be the github file
setwd("/Users/alainastockdill/UMR-TDA-2021/pools EDA/AS")

# Upload the data - both vegetation and water data
veg <- read.csv(file = "../pool data/ltrm_vegsrs_data_lat_long.csv")
water <- read.csv(file = "../pool data/ltrm_water_data_lat_long.csv")

# Split the veg and water data into pool 4 (for water, field number 4)
veg4 <- veg %>% 
  filter(POOL == "04")

water4 <- water %>%
  filter(FLDNUM == "1")

# Put year in its own column
water4$DATE <- as.Date(water4$DATE, "%m/%d/%Y")
water4$year <- year(water4$DATE)

# Create a decade categorical data 
water4 <- water4 %>% mutate(DECADE = case_when(year < 2001 ~ "1993-2000",
                                               year > 2000 & year < 2015 ~"2001-2014",
                                               year > 2014 ~ "2015-2020",
                                               TRUE ~ "oops"))

# Begin to sort by quarter to get a sense of how the data changes by season
water4$QUARTER <- quarter(water4$DATE, with_year = FALSE, fiscal_start = 1)

# Split into the upper pool data
veg4U <- veg4 %>%
  filter(MSTRATUM == "BWC-U")

# To split the water data, take everything above the longitude line and 
# select everything that is not stratum 4 (Lake Pepin)
water4U <- water4 %>% 
  filter(LONGITUDE > -92.2 & STRATUM != "4" & STRATUM != "6" & STRATUM != "5")

# Create a set that is just the continuous variables and one with just vars for presentation
water_vars <- c('TP', 'CHLcal', 'SS', 'TURB')
continuous <- c('SHEETBAR', 'CALCZCD','TN','TP','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI')

# Create a map of the data points by LAT and LONG
# Show the split of upper pool 4 on the entire pool data - colored by STRATUM
ggplot(water4, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point(aes(color = STRATUM))

# Create a map of the data points by LAT and LONG
# Show the split of upper pool 4 zoomed in - separate water4U_test to include
#   more data points than in the split - helps us to view that the split is good
water4U_test = water4 %>% 
  filter(LONGITUDE > -92.05)

# Create a map of the data points by LAT and LONG
# includes line showing where the points differ from upper pool and lake pepin
ggplot(data = water4U, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point() +
  geom_hline(yintercept = -92.11, color = "red")


# Upper pool 4 coloring by STRATUM
  ggplot(water4U, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point(mapping = aes(color = STRATUM))


# Find the number of samples total per year by stratum
ggplot(data = water4U, mapping = aes(x = year)) +
  geom_histogram(aes(fill = factor(STRATUM) ), binwidth = 1, color = "black") +
  scale_fill_brewer(palette = "Blues", labels = c("Main channel", "Side channel", "Backwater")) +
  labs(fill = "Stratum",
       title = "Records per year by stratum")
  
# Normalize the records per year by stratum in order to see how the proportions of 
# samples are by each stratum
ggplot(data = water4U, mapping = aes(x = year)) +
  geom_histogram(aes(fill = factor(STRATUM) ), binwidth = 1, color = "black", position = "fill") +
  scale_fill_brewer(palette = "Blues", labels = c("Main channel", "Side channel", "Backwater")) +
  labs(fill = "Stratum",
       title = "Records per year by stratum - normalized ")

# Split data by stratum - creates three subsets
main <- water4U %>% filter(STRATUM == 1)
side <- water4U %>% filter(STRATUM == 2)
backwater <- water4U %>% filter(STRATUM == 3)


### AMBER'S CODE
# BOXPLOTS for the stratum by decade
plotter_box_by_year <- function(var_str, data, facet_bool){
  
  # facet_bool gives if you should facet by STRATUM type
  
  title <- paste("boxplot_by_year", "4U", var_str, sep = "_")
  
  if (facet_bool){title <- paste(title, "_facet")}
  
  title <- paste(title, "png", sep = ".")
  
  if (facet_bool){
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = DECADE, y = !!sym(var_str), group = DECADE)) +
      geom_boxplot() + 
      facet_wrap(~ STRATUM, scales = "free") +
      labs()
  } else {
    data %>%
      filter(!is.na(!!sym(var_str))) %>%
      ggplot(aes(x = DECADE, y = !!sym(var_str), group = DECADE)) +
      geom_boxplot()
  }
  
  ggsave(title)
  
}

# Function that will find the boxplots for each of the water variables
sapply(water_vars, plotter_box_by_year, water4U, T )
plotter_box_by_year("TURB")


# Split into decades to looks at summary stats for each of the water vars
dec_one <- water4U %>% filter(DECADE == "1993-2000")
dec_two <- water4U %>% filter(DECADE == "2001-2014")
dec_three <- water4U %>% filter(DECADE == "2015-2020")

summary(dec_one$CHLcal)
summary(dec_two$CHLcal)
summary(dec_three$CHLcal)

unique(dec_three$DECADE)







  