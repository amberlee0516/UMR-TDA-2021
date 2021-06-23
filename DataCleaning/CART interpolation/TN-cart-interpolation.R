library(tidyverse)
library(lubridate)
library(stringr)
library(caret)
library(rattle)

setwd("/Users/alainastockdill/UMR-TDA-2021/DataCleaning/CART interpolation")
data <- read.csv(file = "cleaned_data.csv")

# Sets the maximum number of nested expressions to be evaluated
options(expressions = 5e5)

# Remove columns for making the splits
data <- subset(data, select = -c(X, FLDNUM))


# Add in the date, year, and season
data <- data %>% 
  mutate(nice_date = mdy(DATE),
         year = year(nice_date),
         season = quarter(nice_date, fiscal_start = 3)) %>%
  select(-SHEETBAR, -nice_date, -DATE, -LOCATCD)

# Create a new data set that removes the na values
data_rmna = data %>% 
  filter_at(vars(names(data)), all_vars(!is.na(.)))

# Create a new data set that contains only the values with missing data 
test <- data %>%
  filter_at(vars(names(data)), any_vars(is.na(.)))

# Set a random seed
set.seed(4321)

# TN model
fit_control <- caret::trainControl(method = "cv", number = 10)
tr.TN <- caret::train(TN ~ .,
                      data = (data_rmna %>% sample_frac(0.5)),
                      method = "rpart2",
                      trControl = fit_control,
                      tuneGrid = data.frame(maxdepth = (1:20)))

fancyRpartPlot(tr.TN$finalModel, main = "TN all years")
plot(tr.TN, main = "TN all years")


# Split by year and season
make_year_tuples <- function(index, year_partition) {
  return(c(year_partition[index], year_partition[index + 1]))
}


# Creates trees by groups of years 
tree_by_years <- function(year_tuple, water_data) {
  water_data <- water_data %>% filter(year >= year_tuple[1] &
                                      year <= year_tuple[2])
  
  fit_control <- caret::trainControl(method = "cv")
  
  tr.TN <- caret::train(TN ~ .,
                       data = water_data,
                       method = "rpart2",
                       trControl = fit_control,
                       tuneGrid = data.frame(maxdepth = (1:20)))
  
  return(tr.TN)
}


# Create trees by season
tree_by_season <- function(season, min_year, max_year, year_interval, water_data) {
  year_partition <- seq(min_year, max_year, year_interval)
  
  year_tuples <- lapply(1: (length(year_partition) - 1), make_year_tuples, year_partition)
  
  water_data <- water_data %>% filter(season == season)
  
  tree_models <- lapply(year_tuples, tree_by_years, water_data)
  
  return(tree_models)

}



# Run the trees by seasons and print the trees and the RMSE plots
trees.sp.2000.2020 <- tree_by_season(1, 2000, 2020, 5, data_rmna)

lapply(1:4, function(x) return(fancyRpartPlot(trees.sp.2000.2020[[x]]$finalModel,
                                               main = paste(as.character(x)) )))

lapply(1:4, function(x) return(plot(trees.sp.2000.2020[[x]],
                                              main = paste(as.character(x)) )))

test_data = data_rmna %>% sample_frac(.25)

# Make the actual predictions
# I'm guessing that
predict <- tr.TN %>% predict(test)

View(predict)
data.frame( RMSE = RMSE(predict, test$TN))
