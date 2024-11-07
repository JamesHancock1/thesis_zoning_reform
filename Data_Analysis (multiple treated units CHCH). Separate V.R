library(scpi)
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)

CHCH_combined_data <- combined_data %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "Lower Hutt City") %>%
  filter(TA != "Auckland") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District") %>%
  filter(TA != "Hurunui District") %>%
  filter(TA != "Ashburton District")

######################################################
## CHRISTCHURCH + WAIMAKARIRI + SELWYN
######################################################

CHCH_combined_data_demeaned <- CHCH_combined_data %>%
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

scm_CH1 <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                       treatment.var = "treatment", time.var = "Year", constant = TRUE,
                       cointegrated.data = TRUE, features = list("demeaned_outcome"))

### SIMPLEX
res_simplex_CHCH <- scest(scm_CH1, w.constr = list("name" = "simplex"))
summary(res_simplex_CHCH)

scplotMulti(res_simplex_CHCH, type = "series", joint = TRUE)

respi_simplex_CHCH <- scpi(
  scm_CH1, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_simplex_CHCH, type = "series")

# plot errors
scplotMulti(respi_simplex_CHCH, type = "series", joint = TRUE)

### LASSO
res_lasso_CHCH <- scest(scm_CH1, w.constr = list("name" = "lasso"))
summary(res_lasso_CHCH)
scplotMulti(res_lasso_CHCH)

respi_lasso_simple_CHCH <- scpi(
  scm_CH1, 
  w.constr = list("name" = "lasso"), 
  V = "separate",
  cores = 1, 
  sims = 100,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_lasso_simple_CHCH, type = "series")

# plot errors
scplotMulti(respi_lasso_simple_CHCH, type = "series", joint = TRUE)


########################################
# average unit treatment effect (\tau_{i.}) using de-meaned data
########################################
df_i._CH <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = TRUE, features = list("demeaned_outcome"),
                     effect = "unit")
### SIMPLEX
res_simplex_i._CH <- scest(df_i._CH, w.constr = list("name" = "simplex"))
scplotMulti(res_simplex_i._CH)

respi_simplex_i._CH <- scpi(df_i._CH, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_simplex_i._CH, type = "series")

# plot treatment  
scplotMulti(respi_simplex_i._CH, type = "series", joint = TRUE)

### LASSO
res_LASSO_i._CH <- scest(df_i._CH, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_i._CH)

respi_LASSO_i._CH <- scpi(df_i._CH, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_i._CH, type = "series")

# plot treatment
scplotMulti(respi_LASSO_i._CH, type = "series", joint = TRUE)

######################################################
# average treatment effect on the treated (\tau_{.k})
######################################################

df_.k_CH <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                     treatment.var = "treatment", time.var = "Year", constant = TRUE,
                     cointegrated.data = TRUE, features = list("demeaned_outcome"),
                     effect = "time", verbose = TRUE)

### SIMPLEX
res_SIMPLEX_.k_CH <- scest(df_.k_CH, w.constr = list("name" = "simplex"))
scplotMulti(res_SIMPLEX_.k_CH)

respi_SIMPLEX_.k_CH <- scpi(df_.k_CH, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_SIMPLEX_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_SIMPLEX_.k_CH, type = "series", joint = TRUE)

### LASSO
res_LASSO_.k_CH <- scest(df_.k_CH, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_.k_CH)

respi_LASSO_.k_CH <- scpi(df_.k_CH, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                       e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_LASSO_.k_CH, type = "series", joint = TRUE)




######################################################

# PRE-TREATMENT + OTHER MATCHING VARIABLES

########################################
# unit-time treatment effect (\tau_{ik})
########################################

scm_CH2 <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                       treatment.var = "treatment", time.var = "Year", constant = TRUE,
                       cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")))

### SIMPLEX
res_simplex_exp_CHCH <- scest(scm_CH2, w.constr = list("name" = "simplex"))
summary(res_simplex_exp_CHCH)

scplotMulti(res_simplex_exp_CHCH, type = "series", joint = TRUE)

respi_simplex_exp_CHCH <- scpi(
  scm_CH2, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_simplex_exp_CHCH, type = "series")

# plot errors
scplotMulti(respi_simplex_exp_CHCH, type = "series", joint = TRUE)

### OLS
res_ols_exp_CH <- scest(scm_CH2, w.constr = list("name" = "ols"))
summary(res_ols_exp_CH)
scplotMulti(res_ols_exp_CH)

respi_OLS_exp_CH <- scpi(
  scm_CH2, 
  w.constr = list("name" = "ols"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_OLS_exp_CH, type = "series")

# plot errors
scplotMulti(respi_OLS_exp_CH, type = "series", joint = TRUE)


### RIDGE
res_ridge_exp_CH <- scest(scm_CH2, w.constr = list("name" = "ridge"))
summary(res_ridge_exp_CH)
scplotMulti(res_ridge_exp_CH)

respi_ridge_exp_CH <- scpi(
  scm_CH2, 
  w.constr = list("name" = "ridge"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_ridge_exp_CH, type = "series")

# plot errors
scplotMulti(respi_ridge_exp_CH, type = "series", joint = TRUE)


### LASSO
res_lasso_exp_CH <- scest(scm_CH2, w.constr = list("name" = "lasso"))
summary(res_lasso_exp_CH)
scplotMulti(res_lasso_exp_CH)

respi_lasso_exp_CH <- scpi(
  scm_CH2, 
  w.constr = list("name" = "lasso"), 
  V = "separate",
  cores = 1, 
  sims = 50,
  e.method = "all",
  verbose = TRUE)

# plot series
scplotMulti(respi_lasso_exp_CH, type = "series")

# plot errors
scplotMulti(respi_lasso_exp_CH, type = "series", joint = TRUE)

########################################
# average unit treatment effect (\tau_{i.}) using de-meaned data
########################################
df_exp_i._CH <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                         treatment.var = "treatment", time.var = "Year", constant = TRUE,
                         cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")),
                         effect = "unit")
### SIMPLEX
res_exp_i._CH <- scest(df_exp_i._CH, w.constr = list("name" = "simplex"))
scplotMulti(res_exp_i._CH)

respi_simplex_exp_i.CH <- scpi(df_exp_i._CH, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                             e.method = "gaussian")

# plot series
scplotMulti(respi_simplex_exp_i._CH, type = "series")

# plot treatment
scplotMulti(respi_simplex_exp_i._CH, type = "series", joint = TRUE)

### OLS
res_OLS_exp_i._CH <- scest(df_exp_i._CH, w.constr = list("name" = "ols"))
scplotMulti(res_OLS_exp_i._CH)

respi_OLS_exp_i._CH <- scpi(df_exp_i._CH, w.constr = list("name" = "ols"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_exp_i._CH, type = "series")

# plot treatment
scplotMulti(respi_OLS_exp_i._CH, type = "series", joint = TRUE)

### RIDGE
res_RIDGE_exp_i._CH <- scest(df_exp_i._CH, w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_exp_i._CH)

respi_RIDGE_exp_i._CH <- scpi(df_exp_i._CH, w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                           e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_exp_i._CH, type = "series")

# plot treatment
scplotMulti(respi_RIDGE_exp_i._CH, type = "series", joint = TRUE)



### LASSO
res_LASSO_exp_i._CH <- scest(df_exp_i._CH, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_exp_i.)

respi_LASSO_exp_i._CH <- scpi(df_exp_i._CH, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                           e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_exp_i._CH, type = "series")

# plot treatment
scplotMulti(respi_LASSO_exp_i._CH, type = "series", joint = TRUE)


######################################################
# average treatment effect on the treated (\tau_{.k})
######################################################

df_exp_.k_CH <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                         treatment.var = "treatment", time.var = "Year", constant = TRUE,
                         cointegrated.data = TRUE, features = list(c("demeaned_outcome", "Actual Population Growth", "Dwelling_Per_Capita", "Household Income Change")),
                         effect = "time", verbose = TRUE)

### SIMPLEX
res_SIMPLEX_exp_.k_CH <- scest(df_exp_.k_CH, w.constr = list("name" = "simplex"))
scplotMulti(res_SIMPLEX_exp_.k_CH)

respi_SIMPLEX_exp_.k_CH <- scpi(df_exp_.k_CH, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
                             e.method = "gaussian")

# plot series
scplotMulti(respi_SIMPLEX_exp_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_SIMPLEX_exp_.k_CH, type = "series", joint = TRUE)

### OLS
res_OLS_exp_.k_CH <- scest(df_exp_.k_CH, w.constr = list("name" = "ols"))
scplotMulti(res_OLS_exp_.k_CH)

respi_OLS_exp_.k_CH <- scpi(df_exp_.k_CH, w.constr = list("name" = "ols"), cores = 1, sims = 50,
                         e.method = "gaussian")

# plot series
scplotMulti(respi_OLS_exp_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_OLS_exp_.k_CH, type = "series", joint = TRUE)

### RIDGE
res_RIDGE_exp_.k_CH <- scest(df_exp_.k_CH, w.constr = list("name" = "ridge"))
scplotMulti(res_RIDGE_exp_.k_CH)

respi_RIDGE_exp_.k_CH <- scpi(df_exp_.k_CH, w.constr = list("name" = "ridge"), cores = 1, sims = 50,
                           e.method = "gaussian")

# plot series
scplotMulti(respi_RIDGE_exp_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_RIDGE_exp_.k_CH, type = "series", joint = TRUE)

### LASSO
res_LASSO_exp_.k_CH <- scest(df_exp_.k_CH, w.constr = list("name" = "lasso"))
scplotMulti(res_LASSO_exp_.k_CH)

respi_LASSO_exp_.k_CH <- scpi(df_exp_.k_CH, w.constr = list("name" = "lasso"), cores = 1, sims = 50,
                           e.method = "gaussian")

# plot series
scplotMulti(respi_LASSO_exp_.k_CH, type = "series")

# plot treatment
scplotMulti(respi_LASSO_exp_.k_CH, type = "series", joint = TRUE)






 

















































































id.var <- "TA"
time.var <- "Year"
unit.tr <- c("Christchurch City", "Waimakariri District", "Selwyn District")
outcome.var <- "demeaned_outcome"
constant

########################################
# unit-time treatment effect (\tau_{ik})
########################################

CH_combined_data_demeaned <- CHCH_combined_data %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

CHCH_df <- scdataMulti(CH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                  treatment.var = "treatment", time.var = "Year", constant = TRUE,
                  cointegrated.data = TRUE, features = list("demeaned_outcome"))

## simplex approach
res_simplex <- scest(CHCH_df, w.constr = list("name" = "simplex"))
summary(res_simplex)
scplotMulti(res_simplex)

respi_CH_simplex <- scpi(
  CHCH_df, 
  w.constr = list("name" = "simplex"), 
  V = "separate",
  u.missp = TRUE,
  cores = 1, 
  sims = 100,
  e.method = "gaussian",
  verbose = TRUE)

# plot series
scplotMulti(respi_CH_simplex, type = "series", scales = "free")

scplotMulti(respi_CH_simplex, type = "treatment", scales = "free", e.out = TRUE)

# plot treatment
scplotMulti(respi_CH_simplex, type = "series", joint = TRUE)

res_lasso   <- scest(CHCH_df, w.constr = list("name" = "lasso"))

res_ridge   <- scest(CHCH_df, w.constr = list("name" = "ridge"))

summary(res_lasso)




######################################################
# CHCH + WMK + SLW

######################################################


CHCH_combined_data_demeaned <- CHCH_combined_data %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

########################################
# unit-time treatment effect (\tau_{ik})
########################################

df <- scdataMulti(CHCH_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                  treatment.var = "treatment", time.var = "Year", constant = FALSE,
                  cointegrated.data = FALSE, features = list(c("demeaned_outcome", "Actual Population Growth")),
                  cov.adj = list(c("constant", "trend")))


res <- scest(df, w.constr = list("name" = "simplex"))
scplotMulti(res)

# base scpi
respi <- scpi(df, w.constr = list("name" = "simplex"), cores = 1, sims = 50,
              e.method = "gaussian")







# plot series
scplotMulti(respi, type = "series")

# plot treatment
scplotMulti(respi, type = "series", joint = TRUE)