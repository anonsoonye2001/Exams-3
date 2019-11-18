# Exam 3
library(tidyverse)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)


load("test3_data.Rdata")
#1. Subset the data frame to include only the named fields. (2.5 points)
names(d)
fields <- names(d[,c(1:19)])
## SUBSETTING: making a df with only the columns we want from the dataset "phy_t"
a<- d[,c(fields)]
a


#2. Sort the data by ‘transect.id’ and then for each by ‘dateTime’ so that the last observation is the lastone in time.
#This will be done in ascending ordre
d <- arrange(d, transect.id, dateTime)
d


