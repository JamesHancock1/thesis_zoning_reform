library(scpi)
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)

# general MTU data set
combined_data <- read_excel("combined_data.xlsx")

combined_data <- combined_data %>%
  mutate(treatment = case_when(
    TA == "Auckland" & Year >= 2013 ~ 1,
    TA == "Lower Hutt City" & Year >= 2018 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2012 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(TA != "New Zealand")


# AKL + LH specific
LH_combined_data <- combined_data %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Christchurch City") %>%
  filter(TA != "Waimakariri District") %>%
  filter(TA != "Selwyn District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District")

######################################################
## AUCKLAND + LOWER HUTT
######################################################

LH_combined_data_demeaned <- LH_combined_data %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()


######################################################
# JUST PRE-TREATMENT VALUES AS MATCHING VARIABLES

########################################
# unit-time treatment effect (\tau_{ik})
########################################

scm_LH1 <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                  treatment.var = "treatment", time.var = "Year", constant = TRUE,
                  cointegrated.data = TRUE, features = list("demeaned_outcome"))

### SIMPLEX
res_simplex_AKL <- scest(scm_LH1, w.constr = list("name" = "simplex"))
summary(res_simplex_AKL)

scplotMulti(res_simplex_AKL, type = "series", joint = TRUE) 

respi_simplex_AKL <- scpi(
  scm_LH1, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "gaussian",
  verbose = TRUE)

# plot series
scplotMulti(respi_simplex_AKL, type = "series")

# plot errors
scplotMulti(respi_simplex, type = "series", joint = TRUE)

### OLS
res_ols<- scest(scm_LH1, w.constr = list("name" = "ols"))
summary(res_ols)
scplotMulti(res_ols)

respi_OLS_simple <- scpi(
  scm_LH1, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 100,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_OLS_simple, type = "series")

# plot errors
scplotMulti(respi_OLS_simple, type = "series", joint = TRUE)


### RIDGE
res_ridge<- scest(scm_LH1, w.constr = list("name" = "ridge"))
summary(res_ridge)
scplotMulti(res_ridge)

respi_ridge_simple <- scpi(
  scm_LH1, 
  w.constr = list("name" = "ridge"), 
  V = "separate",
  cores = 1, 
  sims = 100,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_ridge_simple, type = "series")

# plot errors
scplotMulti(respi_ridge_simple, type = "series", joint = TRUE)


### LASSO
res_lasso<- scest(scm_LH1, w.constr = list("name" = "lasso"))
summary(res_lasso)
scplotMulti(res_lasso)

respi_lasso_simple <- scpi(
  scm_LH1, 
  w.constr = list("name" = "lasso"), 
  V = "separate",
  cores = 1, 
  sims = 100,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_lasso_simple, type = "series")

# plot errors
scplotMulti(respi_lasso_simple, type = "series", joint = TRUE)


########################################
# average unit treatment effect (\tau_{i.}) using de-meaned data
########################################
df_i. <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = TRUE, features = list("demeaned_outcome"),
                     effect = "unit")
### SIMPLEX
res_simplex_i. <- scest(df_i., w.constr = list("name" = "simplex"))
scplotMulti(res_simplex_i.)

respi_simplex_i. <- scpi(df_i., w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                 e.method = "gaussian")

# plot series
scplotMulti(respi_simplex_i., type = "series")

# plot treatment
scplotMulti(respi_simplex_i., type = "series", joint = TRUE)

### OLS
res_OLS_i. <- scest(df_i., w.constr = list("name" = "ols"))
scplotMulti(res_OLS_i.)

respi_OLS_i. <- scpi(df_i., w.constr = list("name" = "ols"), cores = 1, sims = 50,
                 e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_i., type = "series")

# plot treatment
scplotMulti(respi_OLS_i., type = "series", joint = TRUE)

### RIDGE
res_RIDGE_i. <- scest(df_i., w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_i.)

respi_RIDGE_i. <- scpi(df_i., w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                     e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_i., type = "series")

# plot treatment
scplotMulti(respi_RIDGE_i., type = "series", joint = TRUE)



### LASSO
res_LASSO_i. <- scest(df_i., w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_i.)

respi_LASSO_i. <- scpi(df_i., w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_i., type = "series")

# plot treatment
scplotMulti(respi_LASSO_i., type = "series", joint = TRUE)

######################################################
# average treatment effect on the treated (\tau_{.k})
######################################################

df_.k <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = FALSE, features = list("demeaned_outcome"),
                     effect = "time", verbose = TRUE)

### SIMPLEX
res_SIMPLEX_.k <- scest(df_.k, w.constr = list("name" = "simplex"))
scplotMulti(res_SIMPLEX_.k)

respi_SIMPLEX_.k <- scpi(df_.k, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                 e.method = "gaussian")

# plot series
scplotMulti(respi_SIMPLEX_.k, type = "series")

# plot treatment
scplotMulti(respi_SIMPLEX_.k, type = "series", joint = TRUE)

### OLS
res_OLS_.k <- scest(df_.k, w.constr = list("name" = "ols"))
scplotMulti(res_OLS_.k)

respi_OLS_.k <- scpi(df_.k, w.constr = list("name" = "ols"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_.k, type = "series")

# plot treatment
scplotMulti(respi_OLS_.k, type = "series", joint = TRUE)

### RIDGE
res_RIDGE_.k <- scest(df_.k, w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_.k)

respi_RIDGE_.k <- scpi(df_.k, w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_.k, type = "series")

# plot treatment
scplotMulti(respi_RIDGE_.k, type = "series", joint = TRUE)

### LASSO
res_LASSO_.k <- scest(df_.k, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_.k)

respi_LASSO_.k <- scpi(df_.k, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_.k, type = "series")

# plot treatment
scplotMulti(respi_LASSO_.k, type = "series", joint = TRUE)




######################################################

# PRE-TREATMENT + OTHER MATCHING VARIABLES

########################################
# unit-time treatment effect (\tau_{ik})
########################################

scm_LH2 <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                       treatment.var = "treatment", time.var = "Year", constant = TRUE,
                       cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")))

### SIMPLEX
res_simplex_exp <- scest(scm_LH2, w.constr = list("name" = "simplex"))
summary(res_simplex_exp)

scplotMulti(res_simplex_exp, type = "series", joint = TRUE)

respi_simplex_exp <- scpi(
  scm_LH2, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_simplex_exp, type = "series")

# plot errors
scplotMulti(respi_simplex_exp, type = "series", joint = TRUE)

### OLS
res_ols_exp <- scest(scm_LH2, w.constr = list("name" = "ols"))
summary(res_ols_exp)
scplotMulti(res_ols_exp)

respi_OLS_exp <- scpi(
  scm_LH2, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_OLS_exp, type = "series")

# plot errors
scplotMulti(respi_OLS_exp, type = "series", joint = TRUE)


### RIDGE
res_ridge_exp <- scest(scm_LH2, w.constr = list("name" = "ridge"))
summary(res_ridge_exp)
scplotMulti(res_ridge_exp)

respi_ridge_exp <- scpi(
  scm_LH2, 
  w.constr = list("name" = "ridge"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_ridge_exp, type = "series")

# plot errors
scplotMulti(respi_ridge_exp, type = "series", joint = TRUE)


### LASSO
res_lasso_exp <- scest(scm_LH2, w.constr = list("name" = "lasso"))
summary(res_lasso_exp)
scplotMulti(res_lasso_exp)

respi_lasso_exp <- scpi(
  scm_LH2, 
  w.constr = list("name" = "lasso"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_lasso_exp, type = "series")

# plot errors
scplotMulti(respi_lasso_exp, type = "series", joint = TRUE)

########################################
# average unit treatment effect (\tau_{i.}) using de-meaned data
########################################
df_exp_i. <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")),
                     effect = "unit")
### SIMPLEX
res_exp_i. <- scest(df_exp_i., w.constr = list("name" = "simplex"))
scplotMulti(res_exp_i.)

respi_simplex_exp_i. <- scpi(df_exp_i., w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_simplex_exp_i., type = "series")

# plot treatment
scplotMulti(respi_simplex_exp_i., type = "series", joint = TRUE)

### OLS
res_OLS_exp_i. <- scest(df_exp_i., w.constr = list("name" = "ols"))
scplotMulti(res_OLS_exp_i.)

respi_OLS_exp_i. <- scpi(df_exp_i., w.constr = list("name" = "ols"), cores = 1, sims = 50,
                     e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_exp_i., type = "series")

# plot treatment
scplotMulti(respi_OLS_exp_i., type = "series", joint = TRUE)

### RIDGE
res_RIDGE_exp_i. <- scest(df_exp_i., w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_exp_i.)

respi_RIDGE_exp_i. <- scpi(df_exp_i., w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_exp_i., type = "series")

# plot treatment
scplotMulti(respi_RIDGE_exp_i., type = "series", joint = TRUE)



### LASSO
res_LASSO_exp_i. <- scest(df_exp_i., w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_exp_i.)

respi_LASSO_exp_i. <- scpi(df_exp_i., w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_exp_i., type = "series")

# plot treatment
scplotMulti(respi_LASSO_exp_i., type = "series", joint = TRUE)


######################################################
# average treatment effect on the treated (\tau_{.k})
######################################################

df_exp_.k <- scdataMulti(LH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")),
                     effect = "time", verbose = TRUE)

### SIMPLEX
res_SIMPLEX_exp_.k <- scest(df_exp_.k, w.constr = list("name" = "simplex"))
scplotMulti(res_SIMPLEX_exp_.k)

respi_SIMPLEX_exp_.k <- scpi(df_exp_.k, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_SIMPLEX_exp_.k, type = "series")

# plot treatment
scplotMulti(respi_SIMPLEX_exp_.k, type = "series", joint = TRUE)

### OLS
res_OLS_exp_.k <- scest(df_exp_.k, w.constr = list("name" = "ols"))
scplotMulti(res_OLS_exp_.k)

respi_OLS_exp_.k <- scpi(df_exp_.k, w.constr = list("name" = "ols"), cores = 1, sims = 50,
                     e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_exp_.k, type = "series")

# plot treatment
scplotMulti(respi_OLS_exp_.k, type = "series", joint = TRUE)

### RIDGE
res_RIDGE_exp_.k <- scest(df_exp_.k, w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_exp_.k)

respi_RIDGE_exp_.k <- scpi(df_exp_.k, w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_exp_.k, type = "series")

# plot treatment
scplotMulti(respi_RIDGE_exp_.k, type = "series", joint = TRUE)

### LASSO
res_LASSO_exp_.k <- scest(df_exp_.k, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_exp_.k)

respi_LASSO_exp_.k <- scpi(df_exp_.k, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_exp_.k, type = "series")

# plot treatment
scplotMulti(respi_LASSO_exp_.k, type = "series", joint = TRUE)