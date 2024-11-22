# ADI x subjective cognitive function
# IRT: 
# Previous paper examined racism in relation to the index
  # Model this analysis after that
# Prioritze multinomial-logistic regression for the abstract

# Set-up ----
rm(list=ls())
gc()

if (!dir.exists("cleaned")){
  dir.create("cleaned")
}else{
  cat("Directory already exists \n")
}

## Libraries ----
library(haven)
library(MASS)
library(nnet)
library(tidyverse)

## load ----
temp <- read_sas("../exclude1.sas7bdat")

## Clean ---
# From Tanisha, Variables of interest:
# pick variable closest to 2015 (as closest to outcome)
data <- temp %>% 
  dplyr::select(ADI_state_15, ADI_nation_15, # from proposal, sounds like state
         age15,
         cesd05,bmi15,
         Q6_15, Q7_15, Q8_15, Q9_15, Q10_15, Q11_15, # Subjective cog
         Q5sumcat_15,  # Insomnia Severity Index (4 categories)
         cogsum,cogcat,  # Sum of yes to cognitive & category (multinom outcome)
         smoke15cat, # Smoke status: current, past, never
         packycat15, # Cumulative Pack-years category
         ahei2010_01, # Alternative healthy eating index-2010 Score
         # VRFs; 1 if yes, NA if No:
         diabev15,hypdever15,strokev15,mi_ever15,hcholev15,chfever15,cabgever15,
         vigact15, # Vigorous activity?
         educ03, # '1: <12', '2: 12', '3: 13-15', '4: 16', '5: >=17'
         # Received welfare as child/adult
         publicch, publicad,
         
         # wanted:
         # parental education
         heducparent
         # Region of birth
         # region of residence at survey
  ) %>%
  mutate(
    # Create a VRF indicator
    VRF_any = case_when((diabev15 == 1 | hypdever15 == 1 | strokev15 == 1 |
                           mi_ever15 == 1 | hcholev15 == 1 | chfever15 == 1 |
                           cabgever15 == 1) ~ 1,
                        TRUE ~ 0),
    # Create a VRF Score
    VRF_scr = rowSums(pick(diabev15, hypdever15, strokev15, mi_ever15, hcholev15,
                           chfever15, cabgever15), na.rm=TRUE),
    # Change the ADI deciles to quintiles
    ADI_st_quintile = case_when(ADI_state_15 < 3 ~ 1,
                                ADI_state_15 < 5 ~ 2,
                                ADI_state_15 < 7 ~ 3,
                                ADI_state_15 < 9 ~ 4,
                                TRUE ~ 5),
    # Make a factor version of cog_cat
    cogcat_f = as.factor(cogcat),
    cogcat_b = case_when(cogcat == 0 ~ 0,
                         cogcat > 0 ~ 1),
    
    # Create a binary smoke ever variable
    smkever = case_when(smoke15cat < 3 ~ 1,
                        smoke15cat ==3 ~ 0),
    
    # Address NAs in physical activity
    vigact15 = case_when(vigact15 == 9 ~ NA_real_,
                         TRUE ~ vigact15)
  ) %>% 
  mutate_at(c("diabev15", "hypdever15", "strokev15", "mi_ever15", "hcholev15",
              "chfever15", "cabgever15"),
            replace_na, 0)

# Quick Analyses ----
# Treat ordinal as linear:
summary(lm(cogcat ~ ADI_st_quintile, data = data))
# Ordinal Regression
summary(polr(cogcat_f ~ ADI_st_quintile, data = data, Hess = TRUE))
# Polynomial Regression
summary(multinom(cogcat_f ~ ADI_st_quintile, data = data, Hess = TRUE))
# Logistic
summary(glm(cogcat_b ~ ADI_st_quintile, data=data))

# Save ----
saveRDS(data, "cleaned/analytic_set.RDS")

# Next Steps: 
# move to fully adjusted models
# stick with these type of analyses for the abstract (~November)
# focus on IRT for the paper