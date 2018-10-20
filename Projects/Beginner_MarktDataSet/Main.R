# This is the main script for the Markt Data analysis
library(dplyr)
library(tidyverse)
library(rafalib)
library(ggthemes)
library(cowplot)
library(caret)
setwd('C:/Users/gtsol/Documents/DataSciencePath/Projects/Beginner_MarktDataSet')

train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")
# General Outlet Sales distribution
#train %>% ggplot(aes(x = Item_Outlet_Sales)) + geom_histogram(bins = 100, fill = "darkgreen") +
#  theme_economist_white() + xlab('Outlet Sales') + ylab("Count")

# Combine the two datasets
comb <- rbind(train[,1:11],test)
# Fix the data function 

#1. Fix the fat content data
source("Functions_Markt.R")
comb <- fix_data(comb)

#Explore_Data <- function 
source("Functions_Markt.R")
uni_plots <- Uni_EDA(dat = comb)
uni_plots[[1]]
uni_plots[[2]]
# Bivariate analysis
train_new <- data.frame(comb[1:nrow(train),])
train_new <- train_new %>% 
  mutate(Item_Outlet_Sales = train$Item_Outlet_Sales)

source("Functions_Markt.R")
biv_plots <- Bivariate_EDA(train_new)
biv_plots[[1]]
biv_plots[[2]]

#Engineer the new dataset to be studied. Extend the dataset and encode 
#the categorical variables
source("Functions_Markt.R")
comb_plus <- Eng_dat(comb)

