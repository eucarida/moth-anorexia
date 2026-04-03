# This script is the main script for my statistical analisys of my bachelors thesis 
# By: eucarida
# Created: 2026-03-08
# Last updated: 2026-04-03

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
library(beeswarm)
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

# a plot over death and dose

## Creating a binary "did the moth die coloum"
df_moth_wrangel <- df_moth_raw%>% 
  filter(SireID != "NA") %>% 
  mutate(Death_bin = as_factor(if_else(Death_date == "NA", 
                             true = "1", 
                             false = "Dead", 
                             missing = "Alive")))

# checking al of the coloums data type (this is a good thing to remember)
str(df_moth_wrangel)

# changing the order of "importance" in Dose
df_moth_wrangel <- df_moth_wrangel %>% 
  mutate(Dose = factor(Dose,
                       levels = c("1", 
                                  "1/4",
                                  "1/16",
                                  "1/64",
                                  "Control")))


df_moth_wrangel %>% 
  group_by(Dose) %>% 
  ggplot(aes(x = Death_bin,
             fill = SireID)) +
  geom_bar()+
  facet_grid(Dose ~ SireID)+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  labs(title = "Death across half siblings depending on dose",
       x = "Died or lived thorough the experiment",
       y = "Number of indeviduals")

# The once that where best in the controle is also the best across doses

# now filtering out so thet we are only looking at the weiged food data
  df_moth_wrangel_WF <- df_moth_wrangel %>% 
    filter(FoodWeighing == "Yes",
           FinalFoodWeight != "NA",
           InitFoodWeight != "NA") %>% 
    mutate(Food_Weight_Diff = InitFoodWeight - FinalFoodWeight)
# checking for clearly wrong messurments (ie. bellow 0 in value (negative))
df_moth_wrangel_WF %>% 
  filter(Food_Weight_Diff < 0)
# there is one, that is not good as it illustrates a wrong in the weighing
# if one is below 0 that means either that poop was measured or human error
# poop is worse
# REMOVING BAD VALUE:
df_moth_wrangel_WF <- df_moth_wrangel_WF %>%
  filter(Food_Weight_Diff >= 0)

## This is solved forgot to remove NA from initial food weight ##
## there was nothing wrong with the program/code, I'm just stupid ##
# df_moth_wrangel_WF %>%
#   filter(Food_Weight_Diff <= 0,.preserve = T)
# 
# # need do find the probem Larva
# df_temp <- df_moth_wrangel_WF %>% 
#   arrange(desc(Food_Weight_Diff))
# # ID 949 i wrong (increadebly high probebly missed a "." when noteing it down)
# df_temp <- df_moth_wrangel_WF %>% 
#   arrange((Food_Weight_Diff))
# 
# df_temp2 <- df_temp %>% 
#   filter(Food_Weight_Diff >= 0)
# 
# # comparing the removed and existing data sets
# df_temp3 <- df_temp %>% 
#   summarise(LarvaID, Food_Weight_Diff)
# 
# df_temp2 <- df_temp2 %>% 
#   summarise(LarvaID, Food_Weight_Diff)
# 
# setdiff(df_temp3,df_temp2)

# plot
df_moth_wrangel_WF %>% 
  ggplot(aes(x = Dose,
         y = Food_Weight_Diff_Mean)) +
  geom_col()

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
