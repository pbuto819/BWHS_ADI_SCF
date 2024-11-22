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

# Analysis ----

# Initiate lists to store results 

linear_models <- list()
ordinal_model <- list()
polynom_model <- list()
polyfac_model <- list()
logisti_model <- list()

## Simple Models ----

# Get number of ADI for each model
rbind(
  data %>% 
    select(cogcat, ADI_st_quintile) %>% 
    filter(complete.cases(.)) %>% 
    {table(.$ADI_st_quintile, useNA = "ifany")},
  data %>% 
    select(cogcat, ADI_st_quintile) %>% 
    filter(complete.cases(.)) %>% 
    {prop.table(table(.$ADI_st_quintile, useNA = "ifany"))}
) %>%
  round(2)

# Treat ordinal as linear:
linear_models[["m0"]] <- lm(cogcat ~ ADI_st_quintile, 
                            data = data)
# Ordinal Regression
ordinal_model[["o0"]] <- polr(cogcat_f ~ ADI_st_quintile, 
                              data = data, Hess = TRUE)
# Polynomial Regression
polynom_model[["p0"]] <- multinom(cogcat_f ~ ADI_st_quintile, 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)
# Polynomial: ADI as factor
polyfac_model[["p0"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile), 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)
# Logistic Regression
logisti_model[["l0"]] <- glm(cogcat_b ~ ADI_st_quintile, 
                                  data = data)

## Adjusted Models ----

### Model 1: Age, Region of Birth, Parental Education, Public Assistance ----

# Get number of ADI for each model
rbind(
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch) %>% 
    filter(complete.cases(.)) %>% 
    {table(.$ADI_st_quintile, useNA = "ifany")},
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch) %>% 
    filter(complete.cases(.)) %>% 
    {prop.table(table(.$ADI_st_quintile, useNA = "ifany"))}
) %>% round(2)

# Currently missing RoB
linear_models[["m1"]] <- lm(cogcat ~ ADI_st_quintile + 
                              age15 + heducparent + publicch, 
                            data = data)
ordinal_model[["o1"]] <- polr(cogcat_f ~ ADI_st_quintile + 
                                age15 + heducparent + publicch, 
                              data = data, Hess = TRUE)
polynom_model[["p1"]] <- multinom(cogcat_f ~ ADI_st_quintile + 
                                    age15 + heducparent + publicch, 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)
logisti_model[["l1"]] <- glm(cogcat_b ~ ADI_st_quintile + 
                               age15 + heducparent + publicch, 
                             data = data)
polyfac_model[["p1"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile) + 
                                    age15 + heducparent + publicch, 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)

### Model 2: Education, Region of residence, smoking history ----

# Get number of ADI for each model
rbind(
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch,
           educ03) %>% 
    filter(complete.cases(.)) %>% 
    {table(.$ADI_st_quintile, useNA = "ifany")},
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch,
           educ03) %>% 
    filter(complete.cases(.)) %>% 
    {prop.table(table(.$ADI_st_quintile, useNA = "ifany"))}
) %>% round(2)
# Models are successive

# Currently missing Region of residence
linear_models[["m2"]] <- lm(cogcat ~ ADI_st_quintile + 
                              age15 + heducparent + educ03 + 
                              publicch #+ smkever
                            , 
                            data = data)
ordinal_model[["o2"]] <- polr(cogcat_f ~ ADI_st_quintile + 
                                age15 + heducparent + educ03 + 
                                publicch #+ smkever
                              , 
                              data = data, Hess = TRUE)
polynom_model[["p2"]] <- multinom(cogcat_f ~ ADI_st_quintile + 
                                    age15 + heducparent + educ03 + 
                                    publicch #+ smkever
                                  , 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)
logisti_model[["l2"]] <- glm(cogcat_b ~ ADI_st_quintile + 
                               age15 + heducparent + educ03 + 
                               publicch #+ smkever
                             , 
                             data = data)
polyfac_model[["p2"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile) + 
                                    age15 + heducparent + educ03 + 
                                    publicch #+ smkever
                                  , 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)

### Model 3: BMI, DM, HTN, AHEI, SMK, PA, Depress, insom, & Pub Assist ----
# switched from smoking history to smoking use - check in on how to control?

# Get number of ADI for each model
rbind(
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch,
           educ03,
           bmi15, diabev15, hypdever15, ahei2010_01,
           packycat15, vigact15, cesd05, Q5sumcat_15,
           publicad) %>% 
    filter(complete.cases(.)) %>% 
    {table(.$ADI_st_quintile, useNA = "ifany")},
  data %>% 
    select(cogcat, ADI_st_quintile,
           age15, heducparent, publicch,
           educ03,
           bmi15, diabev15, hypdever15, ahei2010_01,
           packycat15, vigact15, cesd05, Q5sumcat_15,
           publicad) %>% 
    filter(complete.cases(.)) %>% 
    {prop.table(table(.$ADI_st_quintile, useNA = "ifany"))}
) %>% round(2)

linear_models[["m3"]] <- lm(cogcat ~ ADI_st_quintile
                            + age15 + heducparent + educ03
                            + bmi15 + diabev15 + hypdever15 + ahei2010_01
                            + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                            + publicch + publicad, 
                            data = data)
ordinal_model[["o3"]] <- polr(cogcat_f ~ ADI_st_quintile
                              + age15 + heducparent + educ03
                              + bmi15 + diabev15 + hypdever15 + ahei2010_01
                              + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                              + publicch + publicad, 
                              data = data, Hess = TRUE)
polynom_model[["p3"]] <- multinom(cogcat_f ~ ADI_st_quintile
                                  + age15 + heducparent + educ03
                                  + bmi15 + diabev15 + hypdever15 + ahei2010_01
                                  + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                                  + publicch + publicad, 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)
logisti_model[["l3"]] <- glm(cogcat_b ~ ADI_st_quintile 
                             + age15 + heducparent + educ03
                             + bmi15 + diabev15 + hypdever15 + ahei2010_01
                             + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                             + publicch + publicad, 
                             data = data)
polyfac_model[["p3"]] <- multinom(cogcat_f ~ as.factor(ADI_st_quintile)
                                  + age15 + heducparent + educ03
                                  + bmi15 + diabev15 + hypdever15 + ahei2010_01
                                  + packycat15 + vigact15 + cesd05 + Q5sumcat_15
                                  + publicch + publicad, 
                                  data = data, Hess = TRUE,
                                  trace = FALSE)

# Summarize ----
results <- list()

## Linear Models ----
# The outcome isn't actually linear

results$linear <- rbind({tidy(linear_models$m0, conf.int = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(linear_models$m1, conf.int = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(linear_models$m2, conf.int = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(linear_models$m3, conf.int = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)))
results$linear$term <- c("Unadj. Model", "Model 1", "Model 2", "Model 3")

## Ordinal ----
# There is some natural ordering to the outcome

results$ordinal <- rbind({tidy(ordinal_model$o0, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(ordinal_model$o1, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(ordinal_model$o2, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(ordinal_model$o3, conf.int = TRUE,
                               exponentiate = TRUE)} %>%
                           filter(grepl("ADI_st_quintile",term)))
results$ordinal$term <- c("Unadj. Model", "Model 1", "Model 2", "Model 3")
results$ordinal$coef.type <- NULL

## Multinomial Models ----
# Outcome does have a natural ordering
# Similar to how it was done previously
results$polynom <- rbind({tidy(polynom_model$p0, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(polynom_model$p1, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(polynom_model$p2, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(polynom_model$p3, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)))
results$polynom$y.level <- rep(c("1-2","3+"),4)
results$polynom$term <- c("Unadj. Model", "Unadj. Model",
                          "Model 1", "Model 1", 
                          "Model 2", "Model 2", 
                          "Model 3", "Model 3")

results$polynom <- results$polynom %>% rename(SCI_Level = y.level)

## Logistic ----
results$logistic<- rbind({tidy(logisti_model$l0, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(logisti_model$l1, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(logisti_model$l2, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)),
                         {tidy(logisti_model$l3, conf.int = TRUE,
                               exponentiate = TRUE)} %>% 
                           filter(grepl("ADI_st_quintile",term)))
results$logistic$term <- c("Unadj. Model", "Model 1", "Model 2", "Model 3")

## Multinomial - Factor ----
results$multi2 <- rbind({tidy(polyfac_model$p0, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(polyfac_model$p1, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(polyfac_model$p2, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)),
                        {tidy(polyfac_model$p3, conf.int = TRUE,
                              exponentiate = TRUE)} %>% 
                          filter(grepl("ADI_st_quintile",term)))

results$multi2 <- results$multi2 %>% 
  mutate(y.level = case_when(y.level == 1 ~ "1-2 Compliants",
                             y.level == 2 ~ "3+ complaints"),
         term = case_when(term == "as.factor(ADI_st_quintile)2" ~ 2,
                          term == "as.factor(ADI_st_quintile)3" ~ 3,
                          term == "as.factor(ADI_st_quintile)4" ~ 4,
                          term == "as.factor(ADI_st_quintile)5" ~ 5)) %>%
  rename(cog_cat_level = y.level,
         ADI_quintile = term)

results$multi2$model <- c(rep("Unadj. Model", 8),
                          rep("Model 1", 8),
                          rep("Model 2", 8),
                          rep("Model 3", 8))

# Save ----
saveRDS(linear_models,"results/linear_models.RDS")
saveRDS(logisti_model,"results/logistic_models.RDS")
saveRDS(ordinal_model,"results/ordinal_models.RDS")
saveRDS(polyfac_model,"results/poly2_models.RDS")
saveRDS(polynom_model,"results/polynomial_models.RDS")
saveRDS(results, "results/initial_results.RDS")
