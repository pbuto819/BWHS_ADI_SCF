# ADI x subjective cognitive function
# Tables

# Set-up ----
rm(list=ls())
gc()

if (!dir.exists("figures")){
  dir.create("figures")
}else{
  cat("Directory already exists \n")
}


## Libraries ----
library(tidyverse)
library(ggplot2)


## Data ----
data <- readRDS("cleaned/analytic_set.RDS")

## Results ----
main_results <- readRDS("results/initial_results.RDS")
strat_results <- readRDS("results/Stratified_Results.RDS")
supp_results <- readRDS("results/revised_education.RDS")

## Parameters ----
m_lvl <- c("Unadj. Model", "Model 1", "Model 2", "Model 3")
m_lvl2<- c("Unadjusted Model", "Model 1", "Model 2", "Model 3")

# Main Results ----
main_results$linear %>% 
  ggplot() +
  geom_pointrange(aes(x=factor(term, levels = m_lvl), 
                      y = estimate, ymin = conf.low, ymax = conf.high)) + 
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed")

main_results$ordinal %>%
  ggplot() +
  geom_pointrange(aes(x=factor(term, levels = m_lvl), 
                      y = estimate, ymin = conf.low, ymax = conf.high)) + 
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed")

main_results$polynom %>%
  group_by(SCI_Level) %>%
  ggplot() +
  geom_pointrange(aes(x=factor(term, levels = m_lvl), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = SCI_Level), 
                  position = position_dodge(width=.2)) + 
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed")

main_results$logistic %>%
  ggplot() +
  geom_pointrange(aes(x=factor(term, levels = m_lvl), 
                      y = estimate, ymin = conf.low, ymax = conf.high)) + 
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 1, linetype="dashed")

png(file = "figures/Overall.png", width = 720)
main_results$multi2%>%
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 1. OR of State Level ADI Quintile on Subjective Cognitive Function") +
  geom_hline(yintercept = 1, linetype="dashed") + 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()


# Made some changes to education, go with the following plot: 
png(file = "figures/Overall.png", width = 720)
supp_results$multi %>%
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 1. OR of State Level ADI Quintile on Subjective Cognitive Function") +
  geom_hline(yintercept = 1, linetype="dashed") + 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()


# Stratified Results ----

## Education ----
data <- data %>% mutate(
  edu_gths = case_when(educ03 <3 ~ 0,
                       educ03>=3 ~ 1,
                       TRUE ~ NA),
  mood = case_when(cesd05 < 16 ~ "Low",
                   cesd05 < 25 ~ "Moderate",
                   cesd05 >=25 ~ "Severe")
)

table(data$edu_gths)
prop.table(table(data$edu_gths))

png("figures/strat_lshs.png", width = 720)
strat_results$level_0 %>% 
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl2)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 2a. OR of State Level ADI Quintile on SCF among those with a highschool education or less") +
  geom_hline(yintercept = 1, linetype="dashed")+ 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()

png("figures/strat_hs.png", width = 720)
strat_results$level_1 %>% 
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl2)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 2b. OR of State Level ADI Quintile on SCF among those with more than a highschool education") +
  geom_hline(yintercept = 1, linetype="dashed")+ 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()

## Mood ----
table(data$mood, useNA='ifany')
table(data$mood, data$ADI_st_quintile)
table(data$mood, data$ADI_st_quintile, data$cogcat_f)

png("figures/strat_goodmood.png", width = 720)
strat_results$Low %>% 
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl2)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 3a. OR of State Level ADI Quintile on SCF among those with a CESD <= 15")+
  geom_hline(yintercept = 1, linetype="dashed")+ 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()

png("figures/strat_modmood.png", width = 720)
strat_results$Moderate %>% 
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl2)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 3b. OR of State Level ADI Quintile on SCF among those with a CESD from 16 to 24")+
  geom_hline(yintercept = 1, linetype="dashed")+ 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()

png("figures/strat_poormood.png", width = 720)
strat_results$Severe %>% 
  group_by(cog_cat_level) %>%
  ggplot() +
  facet_grid(. ~ factor(model, levels = m_lvl2)) + 
  geom_pointrange(aes(x=as.factor(ADI_quintile), 
                      y = estimate, ymin = conf.low, ymax = conf.high,
                      colour = cog_cat_level), 
                  position = position_dodge(width=1)) +
  labs(x = "ADI State Quintile (vs Quintile 1, Most Advantaged)",
       y = "Odds Ratio for Prevalent Subjective Cognitive Impairment",
       colour = "SCI",
       title = "Figure 3c. OR of State Level ADI Quintile on SCF among those with a CESD >= 25") +
  geom_hline(yintercept = 1, linetype="dashed")+ 
  scale_color_manual(values=c("cyan", "navy"))
dev.off()
