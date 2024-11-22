# Stratified Analyses
# Prioritize multinomial-logistic regression 

# Set-up ----
rm(list=ls())
gc()

if (!dir.exists("results")){
  dir.create("results")
}else{
  cat("Directory already exists \n")
}


## Libraries ----
library(haven)
library(MASS)
library(nnet)
library(tidyverse)
library(broom)

## Load ----
data <- readRDS("cleaned/analytic_set.RDS")

## Data Cleaning ----
prop.table(table(data$educ03, useNA="ifany"))

# From SAS format:
# 1='1: <12' 2='2: 12' 3='3: 13-15' 4='4: 16' 5='5: >=17'

data <- data %>% mutate(
  edu_gths = case_when(educ03 <3 ~ 0,
                       educ03>=3 ~ 1,
                       TRUE ~ NA),
  mood = case_when(cesd05 < 16 ~ "Low",
                   cesd05 < 25 ~ "Moderate",
                   cesd05 >=25 ~ "Severe")
)

# Initiate lists
edu_strat <- list()
mood_strat<-list()
results <- list()

# Stratified by Education ----

## Analysis ----

for(i in c(0, 1)){
  # Model 0: Unadjusted
  edu_strat[[paste0("level_",i)]][["p0"]] <- data %>% 
    filter(edu_gths == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile),
             data=.,
             Hess = TRUE,
             trace = FALSE)
  
  # Model 1: early life confounders
  edu_strat[[paste0("level_",i)]][["p1"]] <- data %>% 
    filter(edu_gths == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) + 
               age15 + heducparent + publicch, 
             data = ., Hess = TRUE,
             trace = FALSE)
  
  # Model 3: Clincial
  edu_strat[[paste0("level_",i)]][["p3"]] <- data %>% 
    filter(edu_gths == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) +
               age15 + heducparent + publicch +
               #educ03 +
               bmi15 + diabev15 + hypdever15 + 
               ahei2010_01 + packycat15 + vigact15 + 
               cesd05 + Q5sumcat_15 + publicad, 
             data = ., Hess = TRUE,
             trace = FALSE)
}

## Clean Results ----

for(i in names(edu_strat)){
  # Pull the regression results
  results[[i]] <- rbind({tidy(edu_strat[[i]]$p0, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(edu_strat[[i]]$p1, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        # {tidy(edu_strat[[i]]$p2, conf.int = TRUE,
                        #       exponentiate = TRUE)} %>% 
                        #   filter(grepl("ADI_st_quintile",term)),
                        {tidy(edu_strat[[i]]$p3, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)))
  
  results[[i]] <- results[[i]] %>% 
    mutate(y.level = case_when(y.level == 1 ~ "1-2 Compliants",
                               y.level == 2 ~ "3+ complaints"),
           term = case_when(term == "as.factor(ADI_st_quintile)2" ~ 2,
                            term == "as.factor(ADI_st_quintile)3" ~ 3,
                            term == "as.factor(ADI_st_quintile)4" ~ 4,
                            term == "as.factor(ADI_st_quintile)5" ~ 5)) %>%
    rename(cog_cat_level = y.level,
           ADI_quintile = term) %>% 
    mutate(model = c(rep("Unadjusted Model", 8),
                     rep("Model 1", 8),
                     #rep("Model 2", 8),
                     rep("Model 3", 8)),
           .before=cog_cat_level)
}

nrow(residuals(edu_strat$level_0$p0))
nrow(residuals(edu_strat$level_0$p1))
nrow(residuals(edu_strat$level_0$p3))

nrow(residuals(edu_strat$level_1$p0))
nrow(residuals(edu_strat$level_1$p1))
nrow(residuals(edu_strat$level_1$p3))

# Stratified by "Mood" ----
prop.table(table(data$cesd05, useNA="always")) 
summary(data$cesd05)

# missing for ~30% of participants. Still a good measure? 
# Leaves us with at most 14753 participants (still sizable)

## Analysis ----
for(i in c("Low", "Moderate", "Severe")){
  
  # Model 0: Unadjusted
  mood_strat[[i]][["p0"]] <- data %>% 
    filter(mood == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile),
             data=.,
             Hess = TRUE,
             trace = FALSE)
  
  # Model 1: early life confounders
  mood_strat[[i]][["p1"]] <- data %>% 
    filter(mood == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) + 
               age15 + heducparent + publicch, 
             data = ., Hess = TRUE,
             trace = FALSE)
  
  # Model 2: Education
  mood_strat[[i]][["p2"]] <- data %>% 
    filter(mood == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) +
               age15 + heducparent + publicch + educ03, 
             data = ., Hess = TRUE,
             trace = FALSE)
  
  # Model 3: Clincial
  mood_strat[[i]][["p3"]] <- data %>% 
    filter(mood == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) +
               age15 + heducparent + publicch + educ03 +
               bmi15 + diabev15 + hypdever15 + 
               ahei2010_01 + packycat15 + vigact15 + 
               #cesd05 +
               Q5sumcat_15 + publicad, 
             data = ., Hess = TRUE,
             trace = FALSE)
}

## Clean Results ----

for(i in names(mood_strat)){
  # Pull the regression results
  results[[i]] <- rbind({tidy(mood_strat[[i]]$p0, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(mood_strat[[i]]$p1, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(mood_strat[[i]]$p2, conf.int = TRUE,
                              exponentiate = TRUE)} %>%
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(mood_strat[[i]]$p3, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)))
  
  results[[i]] <- results[[i]] %>% 
    mutate(y.level = case_when(y.level == 1 ~ "1-2 Compliants",
                               y.level == 2 ~ "3+ complaints"),
           term = case_when(term == "as.factor(ADI_st_quintile)2" ~ 2,
                            term == "as.factor(ADI_st_quintile)3" ~ 3,
                            term == "as.factor(ADI_st_quintile)4" ~ 4,
                            term == "as.factor(ADI_st_quintile)5" ~ 5)) %>%
    rename(cog_cat_level = y.level,
           ADI_quintile = term) %>% 
    mutate(model = c(rep("Unadjusted Model", 8),
                     rep("Model 1", 8),
                     rep("Model 2", 8),
                     rep("Model 3", 8)),
           .before=cog_cat_level)
}

nrow(residuals(mood_strat$Low$p0))
nrow(residuals(mood_strat$Low$p1))
nrow(residuals(mood_strat$Low$p2))
nrow(residuals(mood_strat$Low$p3))

nrow(residuals(mood_strat$Moderate$p0))
nrow(residuals(mood_strat$Moderate$p1))
nrow(residuals(mood_strat$Moderate$p2))
nrow(residuals(mood_strat$Moderate$p3))

nrow(residuals(mood_strat$Severe$p0))
nrow(residuals(mood_strat$Severe$p1))
nrow(residuals(mood_strat$Severe$p2))
nrow(residuals(mood_strat$Severe$p3))

saveRDS(results, "results/Stratified_Results.RDS")


# Interaction ----
ixn <- list()

## Analysis ----

### Education ----

# Model 1: early life confounders
ixn[["education"]][["m2"]] <- data %>% 
  multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile)*edu_gths + 
             age15 + heducparent + publicch, 
           data = ., Hess = TRUE,
           trace = FALSE)

# Model 3: Clincial
ixn[["education"]][["m3"]] <- data %>% 
  multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile)*edu_gths +
             age15 + heducparent + publicch +
             bmi15 + diabev15 + hypdever15 + 
             ahei2010_01 + packycat15 + vigact15 + 
             cesd05 + Q5sumcat_15 + publicad, 
           data = ., Hess = TRUE,
           trace = FALSE)

### Mood ----

# Model 3: Clincial
ixn[["mood"]][["m3"]] <- data %>% 
  multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile)*mood +
             age15 + heducparent + publicch + educ03 + 
             bmi15 + diabev15 + hypdever15 + 
             ahei2010_01 + packycat15 + vigact15 + 
             cesd05 + Q5sumcat_15 + publicad, 
           data = ., Hess = TRUE,
           trace = FALSE)

## Clean Interaction results ----

cleaned_ixn <- rbind(
  {tidy(ixn[["education"]]$m2, conf.int = TRUE,
        exponentiate = TRUE)} %>% 
    filter(grepl("ADI_st_quintile",term)),
  {tidy(ixn[["education"]]$m3, conf.int = TRUE,
        exponentiate = TRUE)} %>% 
    filter(grepl("ADI_st_quintile",term)),
  {tidy(ixn[["mood"]]$m3, conf.int = TRUE,
        exponentiate = TRUE)} %>%
    filter(grepl("ADI_st_quintile",term))
) %>% mutate(
  
)

# Sample Size

nrow(residuals(ixn$mood$m3))
