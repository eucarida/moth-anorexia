## This is a short script for determening the avrage weights of "Toppits Baking Cups Bakformar" and to find the weight to aim for when cuting the arva diet
## Created by Eucarida 2025-11-14
## Last dited by Eucarida 2025-11-14

# clean work space ####
rm(list = ls())
gc()


# load libraries (general) ####
library(ggplot2)
library(performance)
library(GGally)
library(gridExtra)
library(grid)
library(gtable)
library(tidyverse)


# read in data ####
df_sheets <- read_csv("Baking_sheets_weight.csv")


# taking the mean of sheet weight ####
sheet_mean <- mean(df_sheets$weight) # 0.313875 grams







