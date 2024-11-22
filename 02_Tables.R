# ADI x subjective cognitive function
# Tables

# Set-up ----
rm(list=ls())
gc()

if (!dir.exists("tables")){
  dir.create("tables")
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


# Descriptive tables ----

# Make a list of continuous variables
numericVarNames <- c("age15", "ahei2010_01","bmi15","cesd05")

# Make a list of categorical variables
catVarnames <- c("cogcat", 
                 "diabev15", "educ03", "heducparent", 
                 "hypdever15", "packycat15", "publicad", 
                 "publicch", "Q5sumcat_15", "vigact15")

# Create placeholders for categorical variables and their values
# initiate an empty vector
catVarNames_byCat <- c()
# Then loop through each categorical variable
for(cat in catVarnames){
  # and identify the unique values of that variable
  uniqueCats <- base::setdiff(unique(data[,cat]),NA)[[cat]]
  # store a combination of "variable|value" in catVarNames_byCat
  catVarNames_byCat <- c(catVarNames_byCat,paste0(cat,"|",uniqueCats))
}

# remove NA categories
catVarNames_byCat <- grep("NA", catVarNames_byCat, value=TRUE, invert=TRUE)

## Table 1 - Overall Univariate ----
get_univariate_table <- function(d){
  # Initiate a df length of the num & categorical var categories
  table_univariate <- 
    data.frame(matrix(nrow=length(c(numericVarNames,catVarNames_byCat)),ncol=2))
  # The columns are the mean/count & SD/Proportion
  colnames(table_univariate) <- c("Mean or Count","SD or Proportion")
  rownames(table_univariate) <- c(numericVarNames,catVarNames_byCat)
  for(var in numericVarNames){
    table_univariate[var,]<-c(mean(d[[var]],na.rm=TRUE),sd(d[[var]],na.rm=TRUE))
  }
  for(var in catVarnames){
    n_not_NA <- sum(!is.na(d[[var]]))
    for(varValue in unique(d[,var])[[var]]){
      n_value <- sum(d[[var]]==varValue,na.rm=TRUE)
      p <- n_value/n_not_NA
      table_univariate[paste0(var,"|",varValue),]<-c(n_value,p*100)
    }
  }
  return(table_univariate)
}

table_univariate_main <- get_univariate_table(data)
table_univariate_main <- round(table_univariate_main,1)

# Ignore the NA categories
NA_cat <- grep("NA", row.names(table_univariate_main), value=TRUE, invert=TRUE)
table_univariate_main <- table_univariate_main[NA_cat,] 

# Order alphabetically 
# Mainly to order levels as may need to rename anyways. 
table_univariate_main <- 
  table_univariate_main[order(row.names(table_univariate_main)),]

write.csv(table_univariate_main, "tables/table1_overall.csv")

## Table 2 - Descriptive Stats ----

# Initiate list to store results
list_tbl_1_grp <- list()

# For each ADI Quintile
for(grp in c(1:5)){
  # Filter to quintile
  temp <- data %>% filter(ADI_st_quintile == grp)
  
  # Get the univariate table 
  list_tbl_1_grp[[paste0("ADI_",grp)]] <- round(get_univariate_table(temp),1)
  
  # Ignore the NA categories
  NA_cat <- grep("NA", 
                 row.names(list_tbl_1_grp[[paste0("ADI_",grp)]]), 
                 value=TRUE,
                 invert = TRUE)
  list_tbl_1_grp[[paste0("ADI_",grp)]] <- 
    list_tbl_1_grp[[paste0("ADI_",grp)]][NA_cat,] 
  
  # Order alphabetically 
  list_tbl_1_grp[[paste0("ADI_", grp)]] <-
    list_tbl_1_grp[[paste0("ADI_",grp)]][order(row.names(list_tbl_1_grp[[paste0("ADI_",grp)]])),]
  
  grp_char <- as.character(grp)
  
  list_tbl_1_grp[[paste0("ADI_",grp)]] <- 
    list_tbl_1_grp[[paste0("ADI_",grp)]] %>% mutate(
      "ADI Quintile {{grp_char}}" := paste0(`Mean or Count`, " (",
                                            `SD or Proportion`,")")
    ) %>% select(-`Mean or Count`, -`SD or Proportion`)
}

table_univariate_group <- bind_cols(list_tbl_1_grp)

table_univariate_group <- table_univariate_group %>%
  mutate(Overall = paste0(table_univariate_main$`Mean or Count`, " (",
                          table_univariate_main$`SD or Proportion`, ")"),
         .before = `ADI Quintile "1"`)
write.csv(table_univariate_group, "tables/Table1_ByADI_Percentages.csv")

# Analytic Tables ----

## Table 3: Main Results ----
# Re-do the Multi-nom table to match the original publication
multinom2_temp <- list()
multinom2_clean<- list()
for(m in unique(supp_results$multi$model)){
  multinom2_temp[[m]] <- supp_results$multi %>% filter(model == m) %>% 
    select(-std.error, -statistic, -model)
  multinom2_temp[[m]] <- multinom2_temp[[m]] %>% 
    pivot_wider(names_from = ADI_quintile,
                values_from = c(estimate, p.value, conf.low, conf.high))
  multinom2_clean[[m]] <- multinom2_temp[[m]] %>% 
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
multinom2_clean <- bind_rows(multinom2_clean)
multinom2_clean <- multinom2_clean %>% 
  mutate(model = c("Unadj Model", "Unadj Model", "Model 1", "Model 1", 
                           "Model 2", "Model 2", "Model 3", "Model 3"),
         .before = cog_cat_level)

write.csv(multinom2_clean, "tables/Table3_OverallResults.csv")

## Table 4:Education Stratified Results ----

strat_clean<- list()

for(i in names(strat_results)){
  for(m in unique(strat_results[[i]]$model)){
    strat_temp <- list()
    # Extract relevant results
    strat_temp[[i]][[m]] <- 
      strat_results[[i]] %>% 
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
  
  if(i %in% c("level_0", "level_1")){
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

# Save Stratified Education Results
write.csv(strat_clean$level_0, "tables/Table4_Edu_lehs.csv")
write.csv(strat_clean$level_1, "tables/Table4_Edu_gths.csv")

# Save Stratified Mood Results
write.csv(strat_clean$Low,     "tables/Table5_mood_low.csv")
write.csv(strat_clean$Moderate,"tables/Table5_mood_mod.csv")
write.csv(strat_clean$Severe,  "tables/Table5_mood_high.csv")
