#####################################
# title: water-data-cleaning
# author: Amber Lee and Alaina Stockdill
# date: 6-28-21
#####################################

# This script will accompany the water_data_cleaning.Rmd file so that the data
# cleaning may be done all at once. This script will also omit detailed explanations
# as to why we chose to clean the code as we did. These details can be found in the
# markdown file.

# Recall that our end goal is to use Python Kepler Mapper on this data to quantify 
# ecological states in the Upper Mississippi River System. To do so, we hope to retain
# as much data as possible while still only using relevant rows and columns of the LTRM 
# data set. Our goal is to first filter the data for these relevant rows and remove 
# any sample values that may be faulty. We will then use this cleaned data to interpolate
# missing values. 

# Here is what steps were taken to clean the LTRM data for our purposes
# 1. Filter for surface level samples.
# 2. Filter for non-fixed site samples (and select 11 important continuous variables 
#    and 7 identifier variables).
# 3. Change variable values with bad QF code values to `NA`.
# 4. Replace negative values in 11 continuous variables with either the minimum 
#    recorded values or 'NA'
# 5. Replace total nitrogen values greater than 25 with 'NA'
# 6. Collapse duplicate `SHEETBAR`s by taking the averages between their variable values


# Load libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(RColorBrewer)
library(kableExtra)

######################## Functions ########################

qfcodes_setNA <- function(qf_A0, qf_864, water_df, 
                          identifier_var, water_var, waterQF_var){
  # qf_A0 is a character vector of the variable qf names for which 
  # A, 0 qf codes are bad
  
  # qf_864 is a character vector of the variable qf names for which 
  # 8, 64 qf codes are bad
  
  # water_df is the entire water_df
  
  # last 3 variables are just for naming
  
  replace_na_qf <- function(qf_str, df, two_badqfval){
    
    # remove QF at the end of qf_str
    var_str <- substr(qf_str, 1, nchar(qf_str)-2) 
    
    if (var_str == "ZMAX") { var_str <- "WDP"}
    
    df <- df %>%
      # !!sym is for non standard evaluation
      mutate(!!sym(var_str) := case_when(!!sym(qf_str) == two_badqfval[1] ~ NA_real_, 
                                         # specify the type of NA correctly
                                         !!sym(qf_str) == two_badqfval[2] ~ NA_real_,
                                         TRUE ~ !!sym(var_str))) %>%
      # other QF code values are fine, keep data
      select(all_of(c("tmp_idx", var_str, qf_str))) # keep sheetbar for joins
    
    return(df)
    
  } 
  # remove_864 <- function(qf_str, df){
  # 
  #   # remove the QF at the end
  #   var_str <- substr(qf_str, 1, nchar(qf_str)-2) 
  # 
  #   df <- df %>% 
  #     mutate(!!sym(var_str) := case_when(!!sym(qf_str) == 8 ~ NA_real_, 
  #                                        # use correct NA type and !!sym for NSE
  #                                        !!sym(qf_str) == 64 ~ NA_real_,
  #                                        TRUE ~ !!sym(var_str))) %>%
  #                                        # remaining QF code values are fine
  #     select(all_of(c("tmp_idx", var_str, qf_str))) # sheetbars for joining
  #     
  # }
  
  remove_A0_df <- bind_cols(lapply(qf_A0, replace_na_qf, water_df, c("A", "0"))) %>%
    rename(tmp_idx = `tmp_idx...1`) %>%
    select(!contains("..."))
  
  remove_864_df <- bind_cols(lapply(qf_864, replace_na_qf, water_df, c(8, 64))) %>%
    rename(tmp_idx = `tmp_idx...1`) %>%
    select(!contains("..."))
  
  fixedqf_df <- inner_join(remove_864_df, remove_A0_df, by = "tmp_idx") %>%
    inner_join(water_df, by = c("tmp_idx", waterQF_var)) %>% 
    # this causes duplicate columns for water_var, which we do want
    # because water_df has the old (wrong) values for water_var
    select(!contains(".y")) %>% # remove the old (wrong) values from water_df
    rename_with(~ gsub(".x", "", .), contains(".x")) %>% # rename
    select(all_of(c(identifier_var, water_var, waterQF_var))) # reorder columns
  
  return(fixedqf_df)
  
}


qfcodes_check <- function(qf_str, df, two_badqfval){
  
  var_str <- substr(qf_str, 1, nchar(qf_str)-2) 
  if (var_str == "ZMAX") {var_str = "WDP"}
  
  badqf_length <- df %>% 
    filter(!!sym(qf_str) == two_badqfval[1] | !!sym(qf_str) == two_badqfval[2]) %>% 
    pull(!!sym(var_str)) %>%
    length()
  
  badqf_sum <- df %>% 
    filter(!!sym(qf_str) == two_badqfval[1] | !!sym(qf_str) == two_badqfval[2]) %>% 
    pull(!!sym(var_str)) %>% 
    is.na(.) %>%
    sum()
  
  if (badqf_length != badqf_sum) {return(qf_str)}
  return(badqf_length == badqf_sum)
  
}


# Collapse the rows with the same SHEETBAR code
row_collapse <- function(data, water_var, identifier_var) {
  
  # sheetbar_counts contains how many rows each sheetbar has
  # sheetbar_dups is all the sheetbars that have multiple rows
  # data_dups is the actual data (rows) of the duplicate sheetbars
  # water_cleaned contains the combined columns
  # data_dups_ids contaings the identifier variable columns for the duplicated rows
  #     these are set aside so that they won't be averaged
  
  sheetbar_counts <- data %>%
    group_by(SHEETBAR) %>%
    dplyr::summarize(count = n()) %>%
    arrange(-count) # 147130
  
  sheetbar_dups <- sheetbar_counts %>% filter(count > 1) # 33
  data_dups <- data[data$SHEETBAR %in% sheetbar_dups$SHEETBAR, ] # 74
  
  # Set aside the rows with sheetbars that only occur once
  water_cleaned <- data[!(data$SHEETBAR %in% sheetbar_dups$SHEETBAR), ]
  
  data_dups_ids <- data_dups[identifier_var]
  
  data_dups <- aggregate(data_dups[water_var], 
                         by = list(data_dups$SHEETBAR), 
                         FUN = mean,
                         na.rm = TRUE,
                         na.action = na.pass)
  colnames(data_dups)[colnames(data_dups) == 'Group.1'] <- 'SHEETBAR'
  
  is.num <-sapply(data_dups, is.numeric)
  data_dups[is.num] <- lapply(data_dups[is.num], round, 2)
  
  data_dups[is.na(data_dups)] <- NA
  
  # Merge the ids back to the water_vars
  data_dups <- unique(merge(data_dups, data_dups_ids, by = 'SHEETBAR'))
  
  # Add the collapsed rows to the cleaned data
  water_cleaned <- rbind(water_cleaned, data_dups) 
  
  return(water_cleaned)
  
}

#####################################################

# Read data
# set working directory to source file location
 setwd("/Users/alainastockdill/UMR-TDA-2021/DataCleaning") 
water20 <- read.csv(file = "../LTRM data/ltrm_water_data_lat_long.csv")


### Important variables and QF codes
water_var <- c('TN','TP','TEMP','DO','TURB',
               'COND','VEL','SS','WDP','CHLcal','SECCHI')

waterQF_var <- paste(water_var, "QF", sep = "")

identifier_var <- c('SHEETBAR', 'DATE', 'LATITUDE', 'LONGITUDE', 'FLDNUM', 'STRATUM', 'LOCATCD')

waterQF_var <- waterQF_var[waterQF_var != "WDPQF" & 
                             waterQF_var != "CHLcalQF"]

waterQF_var <- c(waterQF_var, "ZMAXQF")


### Filter for surface samples and remove fixed sites (SITETYPE == 2)
water20 <- water20 %>% 
filter(CALCZCD == "SF") %>%
  filter(SITETYPE != 2) %>%
  select(all_of(c(identifier_var, water_var, waterQF_var)))


### Replace values with a bad QF code with NA 
qf_A0 <- c("TURBQF", "TEMPQF", "DOQF", "VELQF", "ZMAXQF",
           "SECCHIQF", "CONDQF")

qf_864 <- c("TNQF", "TPQF", "SSQF")

water20$tmp_idx <- 1:nrow(water20)


# Set codes
qfwater20 <- qfcodes_setNA(qf_A0, qf_864, water20,
                           identifier_var, water_var, waterQF_var)


c(unlist(lapply(qf_A0, qfcodes_check, qfwater20, c("A", "0"))),
  unlist(lapply(qf_864, qfcodes_check, qfwater20, c(8, 64))))


### Replace negatives values with the minimum recorded value
# tmp is the most updated cleaned data set
tmp <- mutate( qfwater20, TP = ifelse(TP < 0, .002, TP))
tmp <- mutate( tmp, TN = ifelse(TN < 0, .024, TN))
tmp <- mutate( tmp, SS = ifelse(SS < 0, .02, SS))
tmp <- mutate( tmp, WDP = ifelse(WDP < 0, 0, WDP))
tmp <- mutate( tmp, CHLcal = ifelse(CHLcal < 0, 0.0183, CHLcal))
tmp <- mutate( tmp, TEMP = ifelse(TEMP < 0, 0.1, TEMP))



### Replace the TN outliers with NA
tmp$TN[tmp$TN > 25] <- NA 


### Collapse rows with the same SHEETBAR
cleaned_data <- row_collapse(tmp[c(water_var, identifier_var)], 
                    water_var, identifier_var)

### Save data
write.csv(cleaned_data, "../LTRM data/water_data_qfneg.csv", row.names = FALSE)








