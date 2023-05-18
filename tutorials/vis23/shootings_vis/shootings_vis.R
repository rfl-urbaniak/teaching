library(rethinking)
library(dplyr)
library(tidyr)
library(ggthemes)
library(readr)
library(gganimate)
library(gifski)

# In this tutorial you will have to demonstrate what you have learned previously
# Complete all 3 sections to earn a grade, we will score the visualization's quality
# You don't have to complete it in one sitting 


#The Washington Post collects information about fatal police shootings
#https://www.washingtonpost.com/graphics/investigations/police-shootings-database/
#https://github.com/washingtonpost/data-police-shootings/tree/master/v2

shootings <- read_csv("fatal-police-shootings-data27042023.csv")

head(shootings)
names(shootings)

#1 Explore the data, making appropriate exploratory visualizations 
#(including raw data plots or correlations)


#2 Put forward an interesting hypothesis and visualize and summarize the relevant
#information


#3 Create an animation that will show something meaningfull







