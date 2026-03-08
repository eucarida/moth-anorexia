# This script is the main script for my statistical analisys of my bachelors thesis 
# By: eucarida
# Created: 2026-03-08
# Last updated: 2026-03-08

# clean
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

# theme
theme_set(theme_bw())


# load data 
df_moth_raw <- read_csv("Moth_expt_Nov_2025_Fs_Larvae.csv")


# simple tidy

df_moth_raw %>% 
  ggplot(aes(x = Pupal_sex, 
             fill = SireID)) +
  geom_bar(colour = "black")


df_moth_raw %>% 
  filter(Pupal_sex == c("F","M")) %>% 
  ggplot(aes(x = Pupal_sex, 
             y = Pupal_mass,
             fill = Diet)) +
  geom_col() +
  facet_grid(.~ Dose)
