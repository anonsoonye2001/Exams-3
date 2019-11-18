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

#3. Create a directory to store figures (2 points)
suppressWarnings(dir.create("plots"))

#4. For each transect with a tow type, plot the vertical path the instrument moved through the water
#column using functions from the ‘ggplot2' library. (10 points)
#a. Use 'dateTime' as your x-axis values. Set the x-axis tick interval to be 15 minutes with the label ‘hour:minute’.


#b. Make sure the water surface is at the top of the plot and the deepest depth is at the bottom of the plot
#c. Make the points hollow and outlined in dark blue.
#d. Label each plot with the transect’s name as the title.
#e. Add a smoother to the plot.

