# New script for randomizing treatments in Nov 2025 QG block
# Göteborgs universitet
# Script by Luc Bussiere
# 
# created Nov 14, 2025
# last modified Nov 14, 2025

rm(list = ls())

# libraries ####
library(boot)
library(brms)
library(tidyverse)




#set.seed(2025)  # make results reproducible; remove/change if you want fresh randomization

# Define factors
diets <- c("Good", "Poor")
doses <- c("Control", "1/64", "1/16", "1/4", "1")

# All 10 treatment combos
treatments <- crossing(Diet = diets, Dose = doses)

# Add a flag for whether food needs weighing
treatments <- treatments %>%
  mutate(FoodWeighing = if_else(Dose %in% c("Control", "1/16", "1"), "Yes", "No"))

# Create one 5-larva group per treatment per block (2 blocks = 100 larvae total)
groups <- treatments %>%
  crossing(Block = 1:2) %>%
  group_by(Block) %>%
  mutate(GroupOrder = sample(row_number())) %>%   # randomize group order within each block
  ungroup() %>%
  arrange(Block, GroupOrder) %>%
  mutate(GroupID = row_number())                  # unique ID across all blocks

# Expand each group to 5 larvae
assignments <- groups %>%
  rowwise() %>%
  mutate(LarvaNumber = list(1:5)) %>%
  unnest(LarvaNumber) %>%
  arrange(Block, GroupOrder, LarvaNumber) %>%
  mutate(LarvaID = row_number())  %>% 
  select(LarvaID, Diet, Dose, FoodWeighing) # 1..100 total execution order

# View full randomized assignment
assignments

write.csv(assignments, "random_order1-100.csv")






