# ADI x subjective cognitive function
# Multiple Imputation
# Several missing values used in model 3

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
library(mice)
library(parameters)

## Load ----
# Use the most original data available
data <- read_sas("../exclude1.sas7bdat")


# Data cleaning ----

data <- data %>% select(
  ADI_state_15, # from proposal, sounds like state
  age15,
  cesd05,bmi15,
  Q6_15, Q7_15, Q8_15, Q9_15, Q10_15, Q11_15, # Subjective cog
  Q5sumcat_15,  # Insomnia Severity Index (4 categories)
  packycat15, # Cumulative Pack-years category
  ahei2010_01, # Alternative healthy eating index-2010 Score
  # VRFs; 1 if yes, NA if No:
  diabev15,hypdever15,strokev15,mi_ever15,hcholev15,chfever15,cabgever15,
  vigact15, # Vigorous activity?
  educ03, # '1: <12', '2: 12', '3: 13-15', '4: 16', '5: >=17'
  # Received welfare as child/adult
  publicch, publicad,
  # parental education
  heducparent
) %>% mutate(
  # Address NAs in physical activity
  # CHECK IN WITH MARIA ABOUT THIS
  vigact15 = case_when(vigact15 == 9 ~ NA_real_,
                       TRUE ~ vigact15)
)

## Missing Rates ----

p_missing <- unlist(lapply(data, function(x) sum(is.na(x))))/nrow(data)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# Missing several observations for variables that go into model 3
# Public assistance as a child has about a ~22.3% missing rate (model 1)

# Clincial vars are only listed as 1, assume NA = 0 
data <- data %>%
  mutate_at(c("diabev15", "hypdever15", "strokev15", "mi_ever15", "hcholev15",
              "chfever15", "cabgever15"),
            replace_na, 0)

# Recode categorical and binary variables to factor
data <- data %>% mutate(
  Q5sumcat_15 = as.factor(Q5sumcat_15),
  publicch = as.factor(publicch),
  publicad = as.factor(publicad),
  vigact15 = as.factor(vigact15)
)

# Is it "okay" to impute childhood variables?
# CHECK IN WITH MARIA OR SCOTT


## Prediction Matrix ----

imp <- mice(data, maxit=0)

# Pull predictor matrix and imputation methods
predM <- imp$predictorMatrix # Every variable is used to predict the others
meth  <- imp$method # Only missing vars have methods. All "pmm"

# use all the variables? Check in with Jilly about best practices
# Not imputing the outcome nor the exposure. 

### Define imputation models ----

# Dichotomous Variables
log <- c("publicch", "publicad")

# Ordered Categorical
poly<- c("Q5sumcat_15", "vigact15")

# Do NOT impute
none<- c("bmi15", "educ03")

meth[log] <- "logreg"
meth[poly]<- "polr"
meth[none]<- ""

# Imputation ----

# Impute `data`, creating 5 datasets with the parameters defined above
imp2 <- mice(data, maxit = 5,
             predictorMatrix = predM,
             method = meth, print=FALSE)

# Analysis ----

## Method 1 ----

# Make lOng
long <- mice::complete(imp2, action="long", include=TRUE)

# Some data cleaning 
long <- long %>% mutate(
  cogsum = Q6_15 +  Q7_15 + Q8_15 + Q9_15 + Q10_15 + Q11_15,
  cogcat = as.factor(case_when(cogsum == 0 ~ 0,
                               cogsum < 2 ~ 1,
                               TRUE ~ 2)),
  ADI_st_quintile = case_when(ADI_state_15 < 3 ~ 1,
                              ADI_state_15 < 5 ~ 2,
                              ADI_state_15 < 7 ~ 3,
                              ADI_state_15 < 9 ~ 4,
                              TRUE ~ 5)
)

# Convert to mids
long <- as.mids(long)

# regression 
fit <- with(long,
            multinom(cogcat ~ as.factor(ADI_st_quintile)
                     + age15 + heducparent + educ03
                     + bmi15 + diabev15 + hypdever15 + ahei2010_01
                     + packycat15 + vigact15 
                     + cesd05 + Q5sumcat_15
                     + publicch + publicad, 
                     Hess = TRUE,
                     trace = FALSE))

# View
temp <- summary(pool(fit), conf.int = TRUE)
temp_out <- temp[c(2:5, 23:26), -c(4:6)] %>% 
  mutate(
    clean = paste0(round(estimate,2)," (",
                   round(`2.5 %`, 2),", ",
                   round(`97.5 %`,2),")")
  )

summary(pool(fit), conf.int=TRUE)

# Alternatively
temp2 <- as.data.frame(model_parameters(pool(fit),exponentiate=TRUE)) 


temp2 <- temp2[c(2:5, 23:26),]%>%
  select(Response, Parameter, Coefficient, CI_low, CI_high, p) %>%
  mutate(clean = paste0(round(Coefficient,2)," (",
                        round(CI_low, 2),", ",
                        round(CI_high,2),") ",
                        round(p, 3)))

saveRDS(fit, "results/mice_model.RDS")
saveRDS(temp2,"results/cleaned_mice_table.RDS")
