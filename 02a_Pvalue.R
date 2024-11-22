#table 2 with P-values

# Set-up ----
rm(list=ls())
gc()
library(tidyverse)
library("gtsummary")
## Data ----
data <- readRDS("cleaned/analytic_set.RDS")


# Make a list of continuous variables
numericVarNames <- c("age15", "ahei2010_01","bmi15","cesd05")

# Make a list of categorical variables
catVarnames <- c("cogcat", 
                 "diabev15", "educ03", "heducparent", 
                 "hypdever15", "packycat15", "publicad", 
                 "publicch", "Q5sumcat_15", "vigact15")

data %>% 
  filter(ADI_st_quintile %in% c(1,5)) %>% 
  select(numericVarNames,catVarnames, ADI_st_quintile) %>%
  tbl_summary(
    digits = all_continuous() ~ 3,
    by = ADI_st_quintile,
    type = list(diabev15 ~ "categorical",
                hypdever15 ~ "categorical",
                heducparent ~ "categorical",
                Q5sumcat_15 ~ "categorical",
                packycat15 ~ "categorical",
                educ03 ~ "categorical",
                publicch ~ "categorical",
                vigact15 ~ "categorical",
                cogcat ~ "categorical",
                publicad ~ "categorical",
                age15 ~ "continuous",
                bmi15 ~ "continuous",
                ahei2010_01 ~ "continuous",
                cesd05 ~ "continuous"),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing = "no"
  ) %>%
  add_p(
    test = list(all_continuous()  ~ "t.test",
                all_categorical() ~ "chisq.test"),
    pvalue_fun = function(x){x}
  )

64.526 - 65.101/ (sqrt((7.126^2/3541) + (7.432^2/3980)))
3541 + 3980 - 2