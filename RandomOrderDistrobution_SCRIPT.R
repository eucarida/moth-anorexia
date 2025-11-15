# a script to view if the distrobution of ranomised dataframes for lara feeding
# Göteborg Universitet
# Script by Eucarida (Olof Heyman)

# created Nov 15, 2025 
# last modified Nov 15, 2025 (by Olof Heyman)

rm(list = ls())
gc()

# libraries ####

library(boot)
library(brms)
library(readr)
library(ggplot2)
library(tidyverse)


# theme ####
theme_set(theme_bw())

# read in data ####
random_f100 <- read_csv2("random_order1-100.csv")



# house keeping ####
# none needed as of now


# distrobutions

random_f100 %>% 
  ggplot(aes(x = Diet)) +
  geom_bar()

random_f100 %>% 
  ggplot(aes(x = Dose)) +
  geom_bar()

random_f100 %>% 
  ggplot(aes(x = FoodWeighing)) +
  geom_bar()

# for the 1-100 .csv every thing is as it should