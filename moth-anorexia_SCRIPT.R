# This script is the main script for my statistical analisys of my bachelors thesis 
# By: eucarida
# Created: 2026-03-08
# Last updated: 2026-04-01

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
library(brms)
library(bayesplot)

# theme
theme_set(theme_bw())


# load data 
df_moth_raw <- read_csv("Moth_expt_Nov_2025_Fs_Larvae.csv")


# simple tidy ####

df_moth_raw %>% 
  ggplot(aes(x = Pupal_sex, 
             fill = SireID)) +
  geom_bar(colour = "black")


df_moth_raw %>% 
  filter(Pupal_sex == c("F", "M"),
         Dose == 1,
         ) %>% 
         # FoodWeighing == "Yes",
         # Eclosion_date != is.na(0)) %>% 
  ggplot(aes(x = Pupal_sex,
             fill = SireID)) +
  geom_bar() +
  facet_grid(Diet ~ Dose)


# need more ggplots

# brms ####

# remove the obs with no food wight
df_moth_WF <- df_moth_raw %>% 
  filter(InitFoodWeight != "NA")

# remove the NA from final food wight
df_moth_WF <- df_moth_WF %>% 
  filter(FinalFoodWeight != "NA")

# add a new coloum for the the differenc in food wight
df_moth_WF <- df_moth_WF %>% 
  mutate(DiffFoodWeight = FinalFoodWeight - InitFoodWeight)

# first beys model
fit <- brm(DiffFoodWeight ~ Dose, data = df_moth_WF)

plot(fit)
# there is more loss in the control dose then the 1 dose

bayesplot_grid(
  pp_check(fit, type = "stat", stat = "mean")
  )

#dont work when catagorical data (fix???)
stanplot(fit)
mcmc_plot(fit)



# this dose not work in this way, need to understand it better to do it the way we did it before
# summary(fit)
# 
# df_tibb_test <- tibble(Dose = levels(factor(df_moth_WF$Dose)))
# 
# preds_matrix_WF <- predict(fit,
#                            newdata = df_tibb_test,
#                            int = "c")
# df_preds_WF <- bind_cols(df_tibb_test,
#                          as_tibble(preds_matrix_WF))
# 
# df_preds_WF %>% 
#   ggplot(aes(x = Dose)) +
#   geom_bar()
