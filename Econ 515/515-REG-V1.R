# Install and load necessary packages 
install.packages(c("readxl","did", "dplyr", "ggplot2", 
                   "lmtest", "panelr", "plm",
                   "tidyverse", "stargazer"))
  library(readxl)
  library(did)
  library(dplyr)
  library(ggplot2)
  library(lmtest)
  library(panelr)
  library(plm)
  library(tidyverse)
  library(stargazer)

# Set working directory: put all data files into the folder you specify here
## Change "" to the file path name for your computer
setwd("/Users/ashley/Desktop/515 Project")

# Read in raw data files
raw_dat <- read_excel("./mostrecent.xlsx", sheet = "Sheet1", col_names = TRUE, na = "NaN")


# Create Vacancy Rate Variable
raw_dat <- raw_dat %>%
  mutate(vac_rate = vac_tot / res_tot)
  
# Create Group Dummy
raw_dat <- raw_dat %>%
  mutate(group = ifelse(treated == 1, 4, 0))

# Create Interactions between Control Variables and Time Trend
raw_dat <- raw_dat %>%
  mutate(home_val_trend = med_home_val*year)

raw_dat <- raw_dat %>%
  mutate(inc_trend = med_hh_inc*year)

raw_dat <- raw_dat %>%
  mutate(lfpr_trend = lfpr*year)

raw_dat <- raw_dat %>%
  mutate(snap_trend = snap*year)

raw_dat <- raw_dat %>%
  mutate(pop_trend = pop*year)

# Create Data Subsets for the different regions we will test
df_a <- subset(raw_dat, county == "Alameda")
df_la <- subset(raw_dat, city == "OAKLAND" | city == "LOS ANGELES")
df_lb <- subset(raw_dat, city == "OAKLAND" | city == "LONG BEACH")
df_sd <- subset(raw_dat, city == "OAKLAND" | city == "SAN DIEGO")
df_sf <- subset(raw_dat, city == "OAKLAND" | city == "SAN FRANCISCO")
df_anaheim <- subset(raw_dat, city == "OAKLAND" | city == "ANAHEIM")

# Specify that the form of our data is panel data
panel <- panel_data(raw_dat, id = tract, wave = year)
panel_a <- panel_data(df_a, id = tract, wave = year)
panel_la <- panel_data(df_la, id = tract, wave = year)
panel_lb <- panel_data(df_lb, id = tract, wave= year)
panel_sd <- panel_data(df_sd, id = tract, wave = year)
panel_sf <- panel_data(df_sf, id = tract, wave = year)
panel_anaheim <- panel_data(df_anaheim, id = tract, wave = year)
  # Can check other large/medium-size cities in CA that are not near Oakland

# Check for Parallel Trend Pre-Treatment: Avg # of Days Vacant Oak + Los Angeles
la_attgt <- att_gt(yname = "avg_vac_days",
                   tname = "year",
                   idname = "tract",
                   gname = "group",
                   xformla = ~med_home_val + med_hh_inc + lfpr + snap + pop,
                   data = df_la,
                   panel = TRUE,
                   allow_unbalanced_panel = TRUE
)
summary(la_attgt)
ggdid(la_attgt) # Parallel trend is confirmed: p-val > 0.05

# Check for Parallel Trend Pre-Treatment: Vacancy Rate Oak + Los Angeles
la_attgt2 <- att_gt(yname = "vac_rate",
                   tname = "year",
                   idname = "tract",
                   gname = "group",
                   xformla = ~med_home_val + med_hh_inc + lfpr + snap + pop,
                   data = df_la,
                   panel = TRUE,
                   allow_unbalanced_panel = TRUE
)
summary(la_attgt2)
ggdid(la_attgt2) # Parallel Trend is confirmed: p-val > 0.05

# Check for Parallel Trend Pre-Treatment: Avg # of Days Vacant Alameda County
test_attgt <- att_gt(yname = "avg_vac_days",
                     tname = "year",
                     idname = "tract",
                     gname = "group",
                     xformla = ~med_home_val + med_hh_inc + lfpr + snap + pop,
                     data = df_a,
                     panel = TRUE,
                     allow_unbalanced_panel = TRUE
                     )
summary(test_attgt)
ggdid(test_attgt) # Parallel trend is weak: p-val = 0.07

# Check for Parallel Trend Pre-Treatment: Vacancy Rate Alameda County
test_attgt2 <- att_gt(yname = "vac_rate",
                      tname = "year",
                      idname = "tract",
                      gname = "group",
                      xformla = ~med_home_val + med_hh_inc + lfpr + snap + pop,
                      data = df_a,
                      panel = TRUE,
                      allow_unbalanced_panel = TRUE
                      )
summary(test_attgt2)
ggdid(test_attgt2) # Parallel trend is confirmed: p-val > 0.05

# Will perform above tests on all cities we consider

########### Treatment Effect on Average Number of Vacant Days

  # No Control Variables
  model_a1 <- plm(avg_vac_days ~ treated + time + did,
                  data = panel_a, robust = TRUE)
  bptest(model_a1) # Test for Heteroskedasticity - use robust std errors in all models
  summary(model_a1)

  # Previous Model + Median Home Value 
  model_a2 <- plm(avg_vac_days ~ treated + time + did
                  + med_home_val, data = panel_a, robust = TRUE)
  bptest(model_a2)
  summary(model_a2)
  
  # Previous Model + Home Val Time Trend
  model_a3 <- plm(avg_vac_days ~ treated + time + did
                    + med_home_val + home_val_trend, 
                    data = panel_a, robust = TRUE)
  bptest(model_a3)
  summary(model_a3)
  
  # Previous Model + Med Household Income
  model_a4 <- plm(avg_vac_days ~ treated + time + did
                  + med_home_val + med_hh_inc, data = panel_a, robust = TRUE)
  bptest(model_a4)
  summary(model_a4)
  
  # Previous Model + Household Income Trend
  model_a5 <- plm(avg_vac_days ~ treated + time + did
                  + med_home_val + home_val_trend 
                  + med_hh_inc + inc_trend
                  , data = panel_a, robust = TRUE)
  bptest(model_a5)
  summary(model_a5)
  
  # Previous Model + Population + Population Trend
  model_a6 <- plm(avg_vac_days ~ treated + time
                  + did + med_home_val + home_val_trend 
                  + med_hh_inc + inc_trend + pop + pop_trend
                  , data = panel_a, robust = TRUE)
  bptest(model_a6)
  summary(model_a6)
  
  # Previous Model + SNAP Benefits (Measure for Poverty) + SNAP Trend
  
  model_a7 <- plm(avg_vac_days ~ treated + time
                  + did + med_home_val + home_val_trend + med_hh_inc
                  + inc_trend + pop + pop_trend + snap + snap_trend
                 , data = panel_a, robust = TRUE)
  bptest(model_a7)
  summary(model_a7)
  
  # Previous Model + Labor Force Participation Rate + LFPR Trend
  model_a8 <- plm(avg_vac_days ~ treated + time
                  + did + med_home_val + home_val_trend + med_hh_inc
                  + inc_trend + pop + pop_trend + snap + snap_trend
                  + lfpr + lfpr_trend
                  , data = panel_a, robust = TRUE)
  bptest(model_a8)
  summary(model_a8)
  
  # Will test more models with other cities as control groups
  # Will test different combinations of control variables, not just ones shown
  
# Example - Write TeX code for regression tables
stargazer(model_a8, dep.var.labels = c("Average Days Vacant"), 
          covariate.labels = c("Treatment Group", "Treatment Period", 
                               "Treatment Effect", "Median Home Value (USD)",
                               "Med Home Value * Year", 
                               "Median Household Income (USD)", 
                               "Med Income * Year",
                               "Population", "Population * Year",
                               "Proportion of Households Receiving SNAP",
                               "SNAP * Year", "Labor Force Participation Rate", 
                               "LFPR * Year"),
          omit.stat = c("LL", "ser", "f"), no.space=TRUE)


############ Vacancy Rate Models

  # No Control Variables
  model_b1 <- plm(vac_rate ~ treated + time + did,
                 data = panel_a, robust = TRUE)
  bptest(model_b1)
  summary(model_b1)
  
  # Previous Model + Median Home Value
  model_b2 <- plm(vac_rate ~ treated + time + did
                  + med_home_val, data = panel_a, robust = TRUE)
  bptest(model_b2)
  summary(model_b2)
  
  # Previous Model + Home Val Time Trend
  model_b3 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  , data = panel_a , robust = TRUE)
  bptest(model_b3)
  summary(model_b3)
  
  # Previous Model + Median Household Income
  model_b4 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  + med_hh_inc
                  , data = panel_a , robust = TRUE)
  bptest(model_b4)
  summary(model_b4)
  
  # Previous Model + Income Trend
  model_b5 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  + med_hh_inc + inc_trend
                  , data = panel_a , robust = TRUE)
  bptest(model_b5)
  summary(model_b5)
  
  # Previous Model + Population + Pop Trend
  model_b6 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  + med_hh_inc + inc_trend + pop + pop_trend
                  , data = panel_a , robust = TRUE)
  bptest(model_b6)
  summary(model_b6)
  
  # Previous Model + SNAP + SNAP Trend
  model_b7 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  + med_hh_inc + inc_trend + pop + pop_trend
                  + snap + snap_trend
                  , data = panel_a , robust = TRUE)
  bptest(model_b7)
  summary(model_b7)
  
  # Previous Model + LFPR + LFPR Trend
  model_b8 <- plm(vac_rate ~ treated + time + did
                  + med_home_val + home_val_trend
                  + med_hh_inc + inc_trend + pop + pop_trend
                  + lfpr + lfpr_trend
                  , data = panel_a , robust = TRUE)
  bptest(model_b8)
  summary(model_b8)



