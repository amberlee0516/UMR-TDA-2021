##################################
## R Bootcamp - Packet 2 Script ##
##################################
#
# Highlight the relevant lines of code and ->RUN 
#
# Remember to set the working directory:
# Session -> Set Working Directory -> To Source Location
library(tidyverse) 

#############################
## LTRM Data
#############################
# https://umesc.usgs.gov/data_library/fisheries/fish1_query.shtml
ltrm <- read_csv("ltrm_fish_data.csv")

# Using the str function to look at the structure of an object
str(ltrm)

names(ltrm)

ltrm["catch"]

ltrm$catch

############################
# Visualization of a Dataset
############################
ggplot(data = ltrm) + 
  geom_point(mapping = aes(x = length, y = weight, color = vegd, alpha = 0.5))

# Only gizzard shad:

unique(ltrm$fishcode)

ltrm_gzsd <- ltrm %>% 
  filter(fishcode == "GZSD") 

ggplot(data = ltrm_gzsd) + 
  geom_point(mapping = aes(x = length, y = weight, color = vegd))

# The last two commands can be combined:
ltrm %>% 
  filter(fishcode == "GZSD") %>%
  ggplot() + 
  geom_point(mapping = aes(x = length, y = weight, color = vegd))

# When working with dates and years it may be needed to
# convert date into new format
ltrm$fdate <- as.Date(ltrm$fdate, "%m/%d/%Y")
# Then pull year and add it as a new column
ltrm <- ltrm %>% mutate(year = year(fdate))

ltrm %>%
  filter(year == 2014) %>%
  ggplot(., aes(x = length)) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  labs(x = "length (in mm)",
       y = "length frequency",
       title = "Length Frequency",
       subtitle = "Year: 2014")

ltrm %>%
  filter(fishcode == "GZSD") %>%
  ggplot(., aes(x = length)) +
  geom_histogram(aes(y = stat(count)), bins = 30) +
  facet_wrap(~ year, nrow = 4) +
  labs(x = "length (in mm)",
       y = "count",
       title = "Counts",
       subtitle = "Gizzard Shad, 2010-2020") 

ggplot(data = ltrm_gzsd) + 
  geom_point(mapping = aes(x = temp, y = length))

ggplot(data = ltrm_gzsd) + 
  geom_smooth(mapping = aes(x = temp, y = length))

# And now together
ggplot(data = ltrm_gzsd) + 
  geom_point(mapping = aes(x = temp, y = length)) +
  geom_smooth(mapping = aes(x = temp, y = length))



names(ltrm)

#######################
# Your Play Area:
#######################


