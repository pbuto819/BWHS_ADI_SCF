# ADI x subjective cognitive function
# IRT: 
# Previous paper examined racism in relation to the index
# Model this analysis after that
# Prioritze multinomial-logistic regression for the abstract

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

## Re-Code ----
data <- data %>% 
  mutate(
    educ03 = case_when(educ03 < 3 ~ 0,
                       educ03 ==3 ~ 1,
                       educ03 > 3 ~ 2,
                       TRUE ~ NA)
  )

# Analysis ----

# Initiate lists to store results 
models <- list()

# Simple Model
# Polynomial: ADI as factor
models[["p0"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile), 
                           data = data, Hess = TRUE,
                           trace = FALSE)

# Model 1: Age, Region of Birth, Parental Education, Public Assistance
models[["p1"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile) + 
                             age15 + heducparent + publicch, 
                           data = data, Hess = TRUE,
                           trace = FALSE)

# Model 2: Education, Region of residence, smoking history
# Models are successive
models[["p2"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile) + 
                             age15 + heducparent + educ03 + 
                             publicch, 
                           data = data, Hess = TRUE,
                           trace = FALSE)

# Model 3: BMI, DM, HTN, AHEI, SMK, PA, Depress, insom, & Pub Assist
models[["p3"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile)
                           + age15 + heducparent + educ03
                           + bmi15 + diabev15 + hypdever15 + ahei2010_01
                           + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                           + publicch + publicad, 
                           data = data, Hess = TRUE,
                           trace = FALSE)





# Stratified by Education ----

## Analysis ----

for(i in c(0, 1, 2)){
  # Model 0: Unadjusted
  models[[paste0("level_",i)]][["p0"]] <- data %>% 
    filter(educ03 == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile),
             data=.,
             Hess = TRUE,
             trace = FALSE)
  
  # Model 1: early life confounders
  models[[paste0("level_",i)]][["p1"]] <- data %>% 
    filter(educ03 == i) %>%
    multinom(formula = cogcat_f ~ as.factor(ADI_st_quintile) + 
               age15 + heducparent + publicch, 
             data = ., Hess = TRUE,
             trace = FALSE)
  
  # Model 3: Clincial
  models[[paste0("level_",i)]][["p3"]] <- data %>% 
    filter(educ03 == i) %>%
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


# Summarize ----
results <- list()
tables <- list()

# Multinomial - Factor 
results$multi <- rbind({tidy(models$p0, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(models$p1, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(models$p2, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(models$p3, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)))

results$multi <- results$multi %>% 
  mutate(y.level = case_when(y.level == 1 ~ "1-2 Compliants",
                             y.level == 2 ~ "3+ complaints"),
         term = case_when(term == "as.factor(ADI_st_quintile)2" ~ 2,
                          term == "as.factor(ADI_st_quintile)3" ~ 3,
                          term == "as.factor(ADI_st_quintile)4" ~ 4,
                          term == "as.factor(ADI_st_quintile)5" ~ 5)) %>%
  rename(cog_cat_level = y.level,
         ADI_quintile = term)

results$multi$model <- c(rep("Unadj. Model", 8),
                         rep("Model 1", 8),
                         rep("Model 2", 8),
                         rep("Model 3", 8))




for(i in c("level_0", "level_1", "level_2")){
  # Pull the regression results
  results[[i]] <- rbind({tidy(models[[i]]$p0, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(models[[i]]$p1, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(models[[i]]$p3, conf.int = TRUE,
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

nrow(residuals(models$level_0$p0))
nrow(residuals(models$level_0$p1))
nrow(residuals(models$level_0$p3))

nrow(residuals(models$level_1$p0))
nrow(residuals(models$level_1$p1))
nrow(residuals(models$level_1$p3))

nrow(residuals(models$level_2$p0))
nrow(residuals(models$level_2$p1))
nrow(residuals(models$level_2$p3))


# Clean Tables ----

for(m in c("Unadj. Model","Model 1","Model 2","Model 3")){
  tables[[m]] <- results$multi %>% 
    filter(model == m) %>% 
    select(-std.error, -statistic, -model) %>% 
    pivot_wider(names_from = ADI_quintile,
                values_from = c(estimate, p.value, conf.low, conf.high)) %>% 
    mutate('Second Quintile' = paste0(round(estimate_2, 2),
                                      " (",round(conf.low_2,2),
                                      ",",round(conf.high_2,2),") ",
                                      "p=",round(p.value_2,2)),
           'Third Quintile' = paste0(round(estimate_3, 2),
                                     " (",round(conf.low_3,2),
                                     ",",round(conf.high_3,2),") ",
                                     "p=",round(p.value_3,2)),
           'Fourth Quintile' = paste0(round(estimate_4, 2),
                                      " (",round(conf.low_4,2),
                                      ",",round(conf.high_4,2),") ",
                                      "p=",round(p.value_4,2)),
           'Fifth Quintile' = paste0(round(estimate_5, 2),
                                     " (",round(conf.low_5,2),
                                     ",",round(conf.high_5,2),") ",
                                     "p=",round(p.value_5,2))) %>%
    select(cog_cat_level, ends_with("Quintile"))
}
tables <- bind_rows(tables)
tables <- tables %>% 
  mutate(model = c("Unadj Model", "Unadj Model", "Model 1", "Model 1", 
                   "Model 2", "Model 2", "Model 3", "Model 3"),
         .before = cog_cat_level)





## Table 4:Education Stratified Results ----

strat_clean<- list()

for(i in c("level_0","level_1","level_2")){
  for(m in unique(results[[i]]$model)){
    strat_temp <- list()
    # Extract relevant results
    strat_temp[[i]][[m]] <- 
      results[[i]] %>% 
      filter(model == m) %>% 
      select(-std.error, -statistic, -model)
    # Make wider
    strat_temp[[i]][[m]] <- strat_temp[[i]][[m]] %>% 
      pivot_wider(names_from = ADI_quintile,
                  values_from = c(estimate, p.value, conf.low, conf.high))
    # Rename
    strat_clean[[i]][[m]] <- strat_temp[[i]][[m]] %>% 
      mutate('Second Quintile' = paste0(round(estimate_2, 2),
                                        " (",round(conf.low_2,2),
                                        ",",round(conf.high_2,2),") ",
                                        "p=",round(p.value_2,2)),
             'Third Quintile' = paste0(round(estimate_3, 2),
                                       " (",round(conf.low_3,2),
                                       ",",round(conf.high_3,2),") ",
                                       "p=",round(p.value_3,2)),
             'Fourth Quintile' = paste0(round(estimate_4, 2),
                                        " (",round(conf.low_4,2),
                                        ",",round(conf.high_4,2),") ",
                                        "p=",round(p.value_4,2)),
             'Fifth Quintile' = paste0(round(estimate_5, 2),
                                       " (",round(conf.low_5,2),
                                       ",",round(conf.high_5,2),") ",
                                       "p=",round(p.value_5,2))) %>%
      select(cog_cat_level, ends_with("Quintile"))
  }
  strat_clean[[i]] <- bind_rows(strat_clean[[i]])
  
  if(i %in% c("level_0", "level_1", "level_2")){
    strat_clean[[i]] <- strat_clean[[i]] %>% 
      mutate(model = c("Unadj Model", "Unadj Model", 
                       "Model 1", "Model 1", 
                       "Model 3", "Model 3"),
             .before = cog_cat_level) %>%
      rename(`Number of Cognitive Complaints` = cog_cat_level)
  }else{
    strat_clean[[i]] <- strat_clean[[i]] %>% 
      mutate(model = c("Unadj Model", "Unadj Model", 
                       "Model 1", "Model 1", 
                       "Model 2", "Model 2", 
                       "Model 3", "Model 3"),
             .before = cog_cat_level) %>%
      rename(`Number of Cognitive Complaints` = cog_cat_level)
  }
  
}

tables_stratified <- bind_rows(strat_clean) %>% 
  mutate(Education = c(rep("Highschool or Less", 6),
                       rep("Some College", 6),
                       rep("College or More", 6)
                       ),
         .before = model
         )

# Save ----
results %>% saveRDS("results/revised_education.RDS")
tables %>% write.csv("tables/updated_education.csv")
tables_stratified %>% write.csv("tables/updated_education_stratified.csv",
                                row.names = FALSE)
