# This script is the main script for my statistical analisys of my bachelors thesis 
# By: eucarida
# Created: 2026-03-08
# Last updated: 2026-04-15

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

# WF ##############################################################

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
  filter(Food_Weight_Diff >= 0,
         Food_Weight_Diff <= 5000)

##############################################################333


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

# WF: more wrangeling ##############################################

# remove usless coloums (to do)
df_moth_wrangel_WF %>% 
  select(!Notes)
 
# add the diet-dose combind coloum
df_moth_wrangel_WF %>% 
  mutate(Diet_Dose = Diet & Dose) %>% 
  summarize(Diet_Dose)



# creat percent of dead alive and dead indeviduals #################


### trying it with Sire-Dose ****
tib_death_percent <- df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet) %>% 
  summarize(tot_offspring = n(),
            nr_dead_offspring = sum(Death_bin == "Dead"),
            nr_alive_offspring = sum(Death_bin == "Alive"),
            percent_alive = nr_alive_offspring / tot_offspring,
            percent_dead = nr_dead_offspring / tot_offspring)



# df_tibb2 join anorexia # # # # # # # # # # # # # # and then plot away


# The differeance in food consumed in comparison to the control
## the reson for useing the 1/16 is to have more data ponts
## that are also more resoneble as many in the 1 dose are dead

df_anorexia <- df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet) %>% 
  summarize(Mean_food_diff = mean(Food_Weight_Diff)) %>% 
  pivot_wider(names_from = Dose,
              values_from = Mean_food_diff) %>% 
  mutate(anorexia = Control - `1/16`)


df_anorexia_survival <- left_join(x = tib_death_percent,
          y = df_anorexia)


## how is anorexia in the 1 dose

df_anorexia_1 <- df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet) %>% 
  summarize(Mean_food_diff = mean(Food_Weight_Diff)) %>% 
  pivot_wider(names_from = Dose,
              values_from = Mean_food_diff) %>% 
  mutate(anorexia = Control - `1`)

df_anorexia_survival_1 <- left_join(x = tib_death_percent,
                                  y = df_anorexia_1)


# ANOREXIA plot #############################################
## basic 1.0 
df_anorexia_survival %>%
  ggplot(aes(x = anorexia,
             y = percent_alive,
             colour = SireID)) +
  geom_point() +
  facet_wrap(Diet ~ Dose) +
  xlim(-0.25,0.25)

## basic 1.1 
df_anorexia_survival_1 %>%
  ggplot(aes(x = anorexia,
             y = percent_alive,
             colour = SireID)) +
  geom_point() +
  facet_wrap(Diet ~ Dose) +
  xlim(-0.25,0.25)

## interesting note is that even in comparison to the 1 dose anorexia is never expressed over all


# ANOREXIA AND DEV TIME #####################################

## changing nonsensical dates into NA
df_moth_wrangel_WF <- df_moth_wrangel_WF %>% 
  mutate(PupalDevTime = as.numeric(PupalDevTime),
         PupalDevTime = if_else(PupalDevTime == "0", 
                                true = NA,
                                false = PupalDevTime,
                                missing = NA),
         PupalDevTime = if_else(PupalDevTime > 20,
                                true = NA,
                                false = PupalDevTime,
                                missing = NA),
         PupalDevTime = if_else(PupalDevTime < 0,
                                true = NA,
                                false = PupalDevTime,
                                missing = NA))

str(df_moth_wrangel_WF)  
df_moth_wrangel_WF %>% 
  count(PupalDevTime)

##  tib the mean dev time for dose, diet and SireID

df_anorexia_devtime <- df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet, PupalDevTime) %>% 
  summarize(Mean_food_diff = mean(Food_Weight_Diff),
            Mean_dev_time = mean(PupalDevTime, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Dose,
              values_from = Mean_food_diff) %>% 
  mutate(anorexia = Control - `1/16`)

df_anorexia_devtime_percent <- left_join(x = tib_death_percent,
                                         y = df_anorexia_devtime)


df_anorexia_devtime_percent %>% 
  ggplot(aes(x = anorexia,
             y = Mean_dev_time)) +
  geom_point() +
  facet_wrap(Diet ~ Dose)


# ANOREXIA X LARVAL WEIGHT ##################################
## tib the mean larval weight [NOT WORKING]
df_anorexia_lweight <- df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet) %>% 
  summarize(Mean_food_diff = mean(Food_Weight_Diff),
            Mean_larval_weight = mean(Larval_wt, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Dose,
              values_from = Mean_food_diff) %>% 
  mutate(anorexia = Control - `1/16`)


df_moth_wrangel_WF %>% 
  group_by(SireID, Dose, Diet) %>% 
  summarize(Mean_larval_weight = mean(Larval_wt, na.rm = TRUE))

df_anorexia_lweight_percent <- left_join(x = tib_death_percent,
                                         y = df_anorexia_lweight)

df_anorexia_lweight_percent %>%
  ggplot(aes(x = anorexia,
             y = Mean_larval_weight)) +
  geom_point()

#############################################################
# plot (bad stuff, clean later)
df_moth_wrangel_WF %>% 
  ggplot(aes(x = Food_Weight_Diff, y = Larval_wt,
             colour = Diet)) +
  geom_point() +
  facet_grid(. ~ Dose)

df_moth_wrangel_WF %>% 
  ggplot(aes(x = Diet, y = Food_Weight_Diff,
             colour = Dose)) +
  geom_point() +
  geom_boxplot()+
  ylim(0,1) +
  facet_grid(. ~ Dose)


# mean survival per for every sire
df_wf_sire <- df_moth_wrangel_WF %>% 
  select(SireID, Death_bin) %>% 
  group_by(SireID, Death_bin) %>% 
  count(SireID, name = "nr_offspring")

# just checking that count does what i think it does
df_temp <- df_moth_wrangel_WF %>% 
  filter(SireID == 112)

# death data and relations 
df_wf_sire <- df_wf_sire%>% 
  pivot_wider(values_from = nr_offspring,
              names_from = Death_bin) %>% 
  mutate(tot_offspring = Alive + Dead) %>% 
  mutate(precent_alive = Alive / tot_offspring,
         precent_dead = Dead / tot_offspring)


# summarise and mean
df_wf_sire %>% 
  summarise(Survival_mean = mean(Alive))

mean(x = df_wf_sire$Alive)
# 60.15385 mean of ofspring alive


# filter over mean survial
df_moth_wrangel_WF %>% 
  add_count(SireID, name = "nr_offspring") %>% 
  filter(nr_offspring > 60.15385) %>% 
  ggplot(aes(x = Food_Weight_Diff,
             y = Larval_wt,
             colour = Diet)) +
  geom_point() +
  facet_wrap(.~ Dose) +
  labs(title = "Food consumed and the weigth of larva",
       subtitle = "over mean of alive offspring from sire",
       x = "Food consumed (g)",
       y = "Larva weight (g)")
  

# no filter
df_moth_wrangel_WF %>% 
  # add_count(SireID, name = "nr_offspring") %>% 
  # filter(nr_offspring > 60.15385) %>% 
  ggplot(aes(x = Food_Weight_Diff,
             y = Larval_wt)) +
  geom_point() +
  facet_wrap(.~ Dose) +
  labs(title = "Food consumed and the weigth of larva",
       x = "Food consumed (g)",
       y = "Larva weight (g)")



df_moth_wrangel_WF %>% 
  ggplot(aes(x = Food_Weight_Diff,
             y = Larval_wt)) +
  geom_point() +
  facet_grid(Death_bin ~ Dose) +
  labs(title = "Food consumed and the weigth of larva",
       x = "Food consumed (g)",
       y = "Larva weight (g)")


 df_moth_wrangel_WF %>% 
   ggplot(aes(x = Dose,
              y = Food_Weight_Diff,
              fill = Death_bin)) +
   geom_boxplot() 
#############################################################
 
 
 
# lm modles ####
lm_moth_wf <- lm(Food_Weight_Diff ~ as_factor(Dose) + Death_bin + SireID,
                 data = df_moth_wrangel_WF)


check_model(lm_moth_wf)


summary(lm_moth_wf)

df_moth_wrangel_WF %>% 
  group_by(SireID, Death_bin) %>%
  add_count(Death_bin) %>% 
  arrange(SireID) %>% 
  pivot_wider(names_from = Death_bin,
              values_from = n) %>% 
  print.data.frame()


# df_moth_wrangel_WF %>%
#   group_by(SireID, Death_bin) %>%
#   summarise(Nr_state = sum(Death_bin %in% c("Alive", "Dead")))
 

# df_moth_wrangel_pre <- 
df_moth_wrangel_WF %>% 
  filter(SireID == "110b") %>% 
  add_count()



  # add_count(SireID, name = "tot_offspring") %>% 
  # group_by(SireID, tot_offspring, Food_Weight_Diff) %>% 
  # mutate(Percent_alive = sum(Death_bin %in% "Alive")/
  #             tot_offspring) %>% 
  # # print.data.frame()
  # ggplot(aes(x = Food_Weight_Diff, 
  #            y = Percent_alive)) +
  # geom_point() +
  # facet_wrap(.~ Death_bin)





str(df_moth_wrangel_pre)








# brms ##### 

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
