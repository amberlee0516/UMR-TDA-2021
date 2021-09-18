# title: "Regression Tree Interpolation"
# author: "Alaina Stockdill"
# date: "07/1/2021"

# This script is to accompany the regression-tree.Rmd which includes detailed
# information on the creation of regression trees. This script can be used to 
# quickly gain the summary stats and errors for regression trees using surrogate
# splits. Regression tress using median imputation will not be included in this 
# script since it has been shown in the regression-trees.Rmd that this method was
# less accurate. This script will also save all of the plots and regression trees
# in the file named CART plots. The errors will be printed at the end. 


# Import libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(rattle)
library(rpart)
library(kableExtra)
library(dplyr)

# Load data
setwd("/Users/alainastockdill/UMR-TDA-2021/InterpolationCode/CART interpolation")
water_data <- read.csv(file = "../../LTRM data/water_data_qfneg.csv")

# Add in the date, year, and season
water_data <- water_data %>% 
  mutate(nice_date = mdy(DATE),
         year = year(nice_date),
         season = quarter(nice_date, fiscal_start = 3)) %>%
  select(-SHEETBAR, -nice_date, -DATE, -LOCATCD)

# Make the FLDNUM and STRATUM categorical variables
water_data$FLDNUM <- as.character(water_data$FLDNUM)
water_data$STRATUM <- as.character(water_data$STRATUM)
water_data$season <- as.character(water_data$season)

water_vars <- list("TP", "TN", "VEL") #, "TURB", "COND","WDP", "SECCHI", "CHLcal", "SS", "TEMP", "DO")

predictor_vars <- c("TP", "TN", "TURB", "COND", "VEL", "WDP", "season", "STRATUM", "FLDNUM", "CHLcal", "SS", "TEMP", "SECCHI", "DO", "year")

rt_data <- water_data[predictor_vars] # regression tree data


### MODEL EVALUTATION FUNCTIONS ###

error_df <- data.frame("Response" = as.character(),
                       "RSME" = as.numeric(),
                       "MAE" = as.numeric(),
                       `Test data size` = as.numeric())

add_errors <- function(error_df, response_var, test_data){
  # the test_data has a column called predicted 
  # that has RF predictions without NAs
  
  test_data <- test_data %>%
    mutate(residual = !!sym(response_var) - predicted,
           residual_sq = residual^2,
           abs_residual = abs(residual))
  
  
  n <- round(dim(test_data)[1], 3)
  rsme <- round(sqrt(sum(test_data$residual_sq)/n), 5)
  mae <- round(sum(test_data$abs_residual)/n, 5)
  
  
  return(rbind(error_df, data.frame("Response" = response_var,
                                    "RSME" = rsme,
                                    "MAE" = mae,
                                    `Test data size` = n)))
  
}

distribution_df <- data.frame("Response" = as.character(),
                              "Mean" = as.numeric(),
                              "Minimum" = as.numeric(),
                              "Quartile 1" = as.numeric(),
                              "Median" = as.numeric(),
                              "Quartile 3" = as.numeric(),
                              "Maximum" = as.numeric())

add_distribution <- function(distribution_df, response_var, test_data){
  
  predictions <- test_data$predicted
  
  return(rbind(distribution_df, 
               data.frame("Response" = response_var,
                          "Mean" = round(mean(predictions), 5),
                          "Minimum" = round(min(predictions), 5),
                          "Quartile 1" = round(quantile(predictions, 0.25), 5),
                          "Median" = round(median(predictions), 5),
                          "Quartile 3" = round(quantile(predictions, 0.50), 5),
                          "Maximum" = round(max(predictions), 5))))
}

### END MODEL EVAL FUNCTIONS ### 


### TN ###

# Create model
water_TN = rt_data %>% filter(!is.na(TN))

set.seed(571)
sample_size = 0.80 * nrow(water_TN)
train_indices <- sample(seq_len(nrow(water_TN)), size = sample_size)

tn_train <- water_TN[train_indices, ]
tn_test <- water_TN[-train_indices, ]

tr.TN_rpart <- rpart(TN ~ .,
                     data = tn_train,
                     method = "anova",
                     control = rpart.control(cp = .011) )

png('CART plots/TN_regression_tree.png')
fancyRpartPlot(tr.TN_rpart, main = "TN_tree")
dev.off()


# Use model to predict test data
tn_test$predicted <- predict(tr.TN_rpart, tn_test)

# Summary stats
error_df <- add_errors(error_df, "TN", tn_test)
distribution_df <- add_distribution(distribution_df, "TN", tn_test)


# Plots
png('CART plots/TN_scatter.png')
ggplot(data = tn_test, aes(x = TN, y = predicted)) +
  geom_point(alpha = 0.25) +
  geom_abline() +
  labs(title = "Predicted versus actual TN values")
dev.off()

png('CART plots/TN_boxplot.png')
ggplot(data = rbind(data.frame(TN = c(tn_test$TN), TN_type = "actual"),
                    data.frame(TN = c(tn_test$predicted), TN_type = "predicted"))) +
  geom_boxplot(aes(x = TN, y = TN_type, group = TN_type)) +
  labs(title = "Distribution of predicted and actual TN values")
dev.off()

png('CART plots/TN_histogram.png')
ggplot(data = tn_test, aes(x = TN - predicted)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of TN residuals", x = "Actual - predicted")
dev.off()



### TP ###

#Create a new data set that removes only the na TN values
water_TP = rt_data %>% filter(!is.na(TP))

set.seed(571)
sample_size = 0.80 * nrow(water_TP)
train_indices <- sample(seq_len(nrow(water_TP)), size = sample_size)

tp_train <- water_TP[train_indices, ]
tp_test <- water_TP[-train_indices, ]

tr.TP_rpart <- rpart(TP ~ .,
                     data = tp_train,
                     method = "anova",
                     control = rpart.control(cp = .011) )


png('CART plots/TP_regression_tree.png')
fancyRpartPlot(tr.TP_rpart, main = "TP_tree")
dev.off()

tp_test$predicted <- predict(tr.TP_rpart, tp_test)

# Summary stats
error_df <- add_errors(error_df, "TP", tp_test)
distribution_df <- add_distribution(distribution_df, "TP", tp_test)

# Plots
png('CART plots/TP_scatter.png')
ggplot(data = tp_test, aes(x = TP, y = predicted)) +
  geom_point(alpha = 0.25) +
  geom_abline() +
  labs(title = "Predicted versus actual TP values")
dev.off()

png('CART plots/TP_boxplot.png')
ggplot(data = rbind(data.frame(TP = c(tp_test$TP), TP_type = "actual"),
                    data.frame(TP = c(tp_test$predicted), TP_type = "predicted"))) +
  geom_boxplot(aes(x = TP, y = TP_type, group = TP_type)) +
  labs(title = "Distribution of predicted and actual TP values")
dev.off()

png('CART plots/TP_histogram.png')
ggplot(data = tp_test, aes(x = TP - predicted)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of TP residuals", x = "Actual - predicted")
dev.off()


### VEL ###

#Create a new data set that removes only the na TN values
water_VEL = rt_data %>% filter(!is.na(VEL))

sample_size = 0.80 * nrow(water_VEL)
train_indices <- sample(seq_len(nrow(water_VEL)), size = sample_size)

vel_train <- water_VEL[train_indices, ]
vel_test <- water_VEL[-train_indices, ]

tr.VEL_rpart <- rpart(VEL ~ .,
                      data = vel_train,
                      method = "anova",
                      control = rpart.control(cp = .011) )


png('CART plots/VEL_regression_tree.png')
fancyRpartPlot(tr.VEL_rpart, main = "VEL_tree")
dev.off()


vel_test$predicted <- predict(tr.VEL_rpart, vel_test)

# Summary stats
error_df <- add_errors(error_df, "VEL", vel_test)
distribution_df <- add_distribution(distribution_df, "VEL", vel_test)


# Plots
png('CART plots/VEL_scatter.png')
ggplot(data = vel_test, aes(x = VEL, y = predicted)) +
  geom_point(alpha = 0.25) +
  geom_abline() +
  labs(title = "Predicted versus actual VEL values")
dev.off()

png('CART plots/VEL_boxplots.png')
ggplot(data = rbind(data.frame(VEL = c(vel_test$VEL), VEL_type = "actual"),
                    data.frame(VEL = c(vel_test$predicted), VEL_type = "predicted"))) +
  geom_boxplot(aes(x = VEL, y = VEL_type, group = VEL_type)) +
  labs(title = "Distribution of predicted and actual VEL values")
dev.off()

png('CART plots/VEL_histogram.png')
ggplot(data = vel_test, aes(x = VEL - predicted)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of VEL residuals", x = "Actual - predicted")
dev.off()

# Print the results
print('Regression tree errors')
print(error_df)

print('Summary stats')
print(distribution_df)


