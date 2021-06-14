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

# Look at the continuous variables
setwd("/Users/alainastockdill/UMR-TDA-2021/pools EDA/AS")

# Upload the data
veg <- read.csv(file = "../pool data/ltrm_vegsrs_data_lat_long.csv")
water <- read.csv(file = "../pool data/ltrm_water_data_lat_long.csv")

# Split the veg and water data into pool 4
veg4 <- veg %>% 
  filter(POOL == "04")

water4 <- water %>%
  filter(FLDNUM == "1")

# Get just the year and put in new coloumn
water4$DATE <- as.Date(water4$DATE, "%m/%d/%Y")
water4$year <- year(water4$DATE)
water4 <- water4 %>% mutate(DECADE = case_when(year < 2001 ~ "1993-2000",
                                               year > 2000 & year < 2015 ~"2001-2014",
                                               year > 2014 ~ "2015-2020",
                                               TRUE ~ "oops"))

# Get the seasons
water4$QUARTER <- quarter(water4$DATE, with_year = FALSE, fiscal_start = 1)

# Split into the upper data
veg4U <- veg4 %>%
  filter(MSTRATUM == "BWC-U")

water4U <- water4 %>% 
  filter(LONGITUDE > -92.2 & STRATUM != "4")

# Create a set that is just the continuous variables and one with just vars for presentation
water_vars <- c('TP', 'CHLcal', 'SS', 'TURB')
continuous <- c('TN','TP','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI')

# Show the split of upper pool 4 on the entire pool data
ggplot(water4, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point(aes(color = STRATUM))

# Show the split of upper pool 4 zoomed in - separate water4U_test to include
#   more data points than in the split - helps us to view that the split is good
water4U_test = water4 %>% 
  filter(LONGITUDE > -92.05)

ggplot(data = water4U, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point() +
  geom_hline(yintercept = -92.11, color = "red")


# upper pool 4 coloring by STRATUM
  ggplot(water4U, mapping = aes(x = LATITUDE, y = LONGITUDE )) + 
  geom_point(mapping = aes(color = STRATUM))


# Find the number of samples total per year
ggplot(data = water4U, mapping = aes(x = year)) +
  geom_histogram(aes(fill = factor(STRATUM) ), binwidth = 1, color = "black") +
  scale_fill_brewer(palette = "Blues", labels = c("Main channel", "Side channel", "Backwater")) +
  labs(fill = "Stratum",
       title = "Records per year by stratum")
  

ggplot(data = water4U, mapping = aes(x = year)) +
  geom_histogram(aes(fill = factor(STRATUM) ), binwidth = 1, color = "black", position = "fill") +
  scale_fill_brewer(palette = "Blues", labels = c("Main channel", "Side channel", "Backwater")) +
  labs(fill = "Stratum",
       title = "Records per year by stratum - normalized ")

# Split data by stratum
main <- water4U %>% filter(STRATUM == 1)
side <- water4U %>% filter(STRATUM == 2)
backwater <- water4U %>% filter(STRATUM == 3)

summary(main)

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

# Look at missing values by stratum


### AMBER'S CODE
# BOXPLOTS
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

sapply(water_vars, plotter_box_by_year, water4U, T )

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



# Split into decades
dec_one <- water4U %>% filter(DECADE == "1993-2000")
dec_two <- water4U %>% filter(DECADE == "2001-2014")
dec_three <- water4U %>% filter(DECADE == "2015-2020")

summary(dec_one$CHLcal)
summary(dec_two$CHLcal)
summary(dec_three$CHLcal)

unique(dec_three$DECADE)








  