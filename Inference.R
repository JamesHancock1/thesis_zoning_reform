# synthetic control values pre-treatment
df_preSC <- as.data.frame(res_simplex_ALL$est.results$Y.pre.fit)
df_clean_preSC <- df_preSC %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value = V1) # Rename V1 to sc_value

# synthetic control values post-treatment
df_postSC <- as.data.frame(res_simplex_ALL$est.results$Y.post.fit)
df_clean_postSC <- df_postSC %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value = V1) # Rename V1 to sc_value

# actual values pre-treatment
df_preRAW <- as.data.frame(res_simplex_ALL$data$Y.pre)
df_clean_preRAW <- df_preRAW %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) # Convert Year to numeric
  

# actual values post-treatment
df_postRAW <- as.data.frame(res_simplex_ALL$data$Y.post)
df_clean_postRAW <- df_postRAW %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric


###################### AUCKLAND

#filter all data sets to just have Akl data
df_clean_preSC
df_clean_postSC
df_clean_preRAW
df_clean_postRAW

df_akl_preSC <- df_clean_preSC %>% filter(City == "Auckland")
df_akl_postSC <- df_clean_postSC %>% filter(City == "Auckland")
df_akl_preRAW <- df_clean_preRAW %>% filter(City == "Auckland")
df_akl_postRAW <- df_clean_postRAW %>% filter(City == "Auckland")


# MSPE: 

df_akl_pre <- merge(df_akl_preRAW, df_akl_preSC, by = "Year", suffixes = c("_raw", "_predicted"))
df_akl_post <- merge(df_clean_postRAW, df_akl_postSC, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_akl_pre <- mean((df_akl_pre$demeaned_outcome - df_akl_pre$sc_value)^2)
mspe_akl_post <- mean((df_akl_post$demeaned_outcome - df_akl_post$sc_value)^2)

# Print the result
print(mspe_akl_pre)
print(mspe_akl_post)

# MSPE-R

MSPER_akl <- (mspe_akl_post/mspe_akl_pre)
print(MSPER_akl)

###################### LOWER HUTT
df_LH_preSC <- df_clean_preSC %>% filter(City == "Lower Hutt City")
df_LH_postSC <- df_clean_postSC %>% filter(City == "Lower Hutt City")
df_LH_preRAW <- df_clean_preRAW %>% filter(City == "Lower Hutt City")
df_LH_postRAW <- df_clean_postRAW %>% filter(City == "Lower Hutt City")

# MSPE: 
df_LH_pre <- merge(df_LH_preRAW, df_LH_preSC, by = "Year", suffixes = c("_raw", "_predicted"))
df_LH_post <- merge(df_clean_postRAW, df_LH_postSC, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_LH_pre <- mean((df_LH_pre$demeaned_outcome - df_LH_pre$sc_value)^2)
mspe_LH_post <- mean((df_LH_post$demeaned_outcome - df_LH_post$sc_value)^2)

# Print the result
print(mspe_LH_pre)
print(mspe_LH_post)

# MSPE-R
MSPER_LH <- (mspe_LH_post/mspe_LH_pre)
print(MSPER_LH)

###################### CHRISTCHURCH
df_CHCH_preSC <- df_clean_preSC %>% filter(City == "Christchurch City")
df_CHCH_postSC <- df_clean_postSC %>% filter(City == "Christchurch City")
df_CHCH_preRAW <- df_clean_preRAW %>% filter(City == "Christchurch City")
df_CHCH_postRAW <- df_clean_postRAW %>% filter(City == "Christchurch City")
# MSPE: 
df_CHCH_pre <- merge(df_CHCH_preRAW, df_CHCH_preSC, by = "Year", suffixes = c("_raw", "_predicted"))
df_CHCH_post <- merge(df_clean_postRAW, df_CHCH_postSC, by = "Year", suffixes = c("_raw", "_predicted"))
# Calculate MSPE
mspe_CHCH_pre <- mean((df_CHCH_pre$demeaned_outcome - df_CHCH_pre$sc_value)^2)
mspe_CHCH_post <- mean((df_CHCH_post$demeaned_outcome - df_CHCH_post$sc_value)^2)

# Print the result
print(mspe_CHCH_pre)
print(mspe_CHCH_post)
# MSPE-R
MSPER_CHCH <- (mspe_CHCH_post/mspe_CHCH_pre)
print(MSPER_CHCH)

###################### WAIMAKARIRI
df_WMK_preSC <- df_clean_preSC %>% filter(City == "Waimakariri District")
df_WMK_postSC <- df_clean_postSC %>% filter(City == "Waimakariri District")
df_WMK_preRAW <- df_clean_preRAW %>% filter(City == "Waimakariri District")
df_WMK_postRAW <- df_clean_postRAW %>% filter(City == "Waimakariri District")
# MSPE: 
df_WMK_pre <- merge(df_WMK_preRAW, df_WMK_preSC, by = "Year", suffixes = c("_raw", "_predicted"))
df_WMK_post <- merge(df_clean_postRAW, df_WMK_postSC, by = "Year", suffixes = c("_raw", "_predicted"))
# Calculate MSPE
mspe_WMK_pre <- mean((df_WMK_pre$demeaned_outcome - df_WMK_pre$sc_value)^2)
mspe_WMK_post <- mean((df_WMK_post$demeaned_outcome - df_WMK_post$sc_value)^2)

# Print the result
print(mspe_WMK_pre)
print(mspe_WMK_post)
# MSPE-R
MSPER_WMK <- (mspe_WMK_post/mspe_WMK_pre)
print(MSPER_WMK)

###################### SELWYN
df_SLW_preSC <- df_clean_preSC %>% filter(City == "Selwyn District")
df_SLW_postSC <- df_clean_postSC %>% filter(City == "Selwyn District")
df_SLW_preRAW <- df_clean_preRAW %>% filter(City == "Selwyn District")
df_SLW_postRAW <- df_clean_postRAW %>% filter(City == "Selwyn District")
# MSPE: 
df_SLW_pre <- merge(df_SLW_preRAW, df_SLW_preSC, by = "Year", suffixes = c("_raw", "_predicted"))
df_SLW_post <- merge(df_clean_postRAW, df_SLW_postSC, by = "Year", suffixes = c("_raw", "_predicted"))
# Calculate MSPE
mspe_SLW_pre <- mean((df_SLW_pre$demeaned_outcome - df_SLW_pre$sc_value)^2)
mspe_SLW_post <- mean((df_SLW_post$demeaned_outcome - df_SLW_post$sc_value)^2)

# Print the result
print(mspe_SLW_pre)
print(mspe_SLW_post)
# MSPE-R
MSPER_SLW <- (mspe_SLW_post/mspe_SLW_pre)
print(MSPER_SLW)

### IN TIME PLACEBO CHECK: RE RUN MODEL FOR PRIOR TWO PERIODS AND CALCULATE CORRESPONDING MSPE RATIOS 



####### One period before: 

combined_data_4bef <- combined_data %>%
  mutate(treatment = case_when(
    TA == "Auckland" & Year >= 2012 ~ 1,
    TA == "Lower Hutt City" & Year >= 2017 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2011 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District") %>%
  filter(TA != "Hurunui District") %>%
  filter(TA != "Ashburton District") 

all_combined_data_demeaned_4bef <- combined_data_4bef %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

scm_all_1bef <- scdataMulti(all_combined_data_demeaned_4bef, id.var = "TA", outcome.var = "demeaned_outcome",
                            treatment.var = "treatment", time.var = "Year", constant = TRUE,
                            cointegrated.data = TRUE, features = list("demeaned_outcome"))
### SIMPLEX
res_simplex_ALL_1bef <- scest(scm_all_1bef, w.constr = list("name" = "simplex"))

# Synthetic control values pre-treatment
df_preSC_1bef <- as.data.frame(res_simplex_ALL_1bef$est.results$Y.pre.fit)
df_clean_preSC_1bef <- df_preSC_1bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value_1bef = V1) # Rename V1 to sc_value_1bef

# Synthetic control values post-treatment
df_postSC_1bef <- as.data.frame(res_simplex_ALL_1bef$est.results$Y.post.fit)
df_clean_postSC_1bef <- df_postSC_1bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value_1bef = V1) # Rename V1 to sc_value_1bef

# Actual values pre-treatment
df_preRAW_1bef <- as.data.frame(res_simplex_ALL_1bef$data$Y.pre)
df_clean_preRAW_1bef <- df_preRAW_1bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) # Convert Year to numeric

# Actual values post-treatment
df_postRAW_1bef <- as.data.frame(res_simplex_ALL_1bef$data$Y.post)
df_clean_postRAW_1bef <- df_postRAW_1bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) # Convert Year to numeric

###################### AUCKLAND
# Filter all datasets to just have Auckland data
df_akl_preSC_1bef <- df_clean_preSC_1bef %>% filter(City == "Auckland")
df_akl_postSC_1bef <- df_clean_postSC_1bef %>% filter(City == "Auckland")
df_akl_preRAW_1bef <- df_clean_preRAW_1bef %>% filter(City == "Auckland")
df_akl_postRAW_1bef <- df_clean_postRAW_1bef %>% filter(City == "Auckland")

# MSPE:
df_akl_pre_1bef <- merge(df_akl_preRAW_1bef, df_akl_preSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_akl_post_1bef <- merge(df_akl_postRAW_1bef, df_akl_postSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_akl_pre_1bef <- mean((df_akl_pre_1bef$demeaned_outcome - df_akl_pre_1bef$sc_value_1bef)^2)
mspe_akl_post_1bef <- mean((df_akl_post_1bef$demeaned_outcome - df_akl_post_1bef$sc_value_1bef)^2)

# Print the result
print(mspe_akl_pre_1bef)
print(mspe_akl_post_1bef)

# MSPE-R
MSPER_akl_1bef <- (mspe_akl_post_1bef / mspe_akl_pre_1bef)
print(MSPER_akl_1bef)

###################### LOWER HUTT
df_LH_preSC_1bef <- df_clean_preSC_1bef %>% filter(City == "Lower Hutt City")
df_LH_postSC_1bef <- df_clean_postSC_1bef %>% filter(City == "Lower Hutt City")
df_LH_preRAW_1bef <- df_clean_preRAW_1bef %>% filter(City == "Lower Hutt City")
df_LH_postRAW_1bef <- df_clean_postRAW_1bef %>% filter(City == "Lower Hutt City")

# MSPE:
df_LH_pre_1bef <- merge(df_LH_preRAW_1bef, df_LH_preSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_LH_post_1bef <- merge(df_LH_postRAW_1bef, df_LH_postSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_LH_pre_1bef <- mean((df_LH_pre_1bef$demeaned_outcome - df_LH_pre_1bef$sc_value_1bef)^2)
mspe_LH_post_1bef <- mean((df_LH_post_1bef$demeaned_outcome - df_LH_post_1bef$sc_value_1bef)^2)

# Print the result
print(mspe_LH_pre_1bef)
print(mspe_LH_post_1bef)

# MSPE-R
MSPER_LH_1bef <- (mspe_LH_post_1bef / mspe_LH_pre_1bef)
print(MSPER_LH_1bef)

###################### CHRISTCHURCH
df_CHCH_preSC_1bef <- df_clean_preSC_1bef %>% filter(City == "Christchurch City")
df_CHCH_postSC_1bef <- df_clean_postSC_1bef %>% filter(City == "Christchurch City")
df_CHCH_preRAW_1bef <- df_clean_preRAW_1bef %>% filter(City == "Christchurch City")
df_CHCH_postRAW_1bef <- df_clean_postRAW_1bef %>% filter(City == "Christchurch City")

# MSPE:
df_CHCH_pre_1bef <- merge(df_CHCH_preRAW_1bef, df_CHCH_preSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_CHCH_post_1bef <- merge(df_CHCH_postRAW_1bef, df_CHCH_postSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_CHCH_pre_1bef <- mean((df_CHCH_pre_1bef$demeaned_outcome - df_CHCH_pre_1bef$sc_value_1bef)^2)
mspe_CHCH_post_1bef <- mean((df_CHCH_post_1bef$demeaned_outcome - df_CHCH_post_1bef$sc_value_1bef)^2)

# Print the result
print(mspe_CHCH_pre_1bef)
print(mspe_CHCH_post_1bef)

# MSPE-R
MSPER_CHCH_1bef <- (mspe_CHCH_post_1bef / mspe_CHCH_pre_1bef)
print(MSPER_CHCH_1bef)

###################### WAIMAKARIRI
df_WMK_preSC_1bef <- df_clean_preSC_1bef %>% filter(City == "Waimakariri District")
df_WMK_postSC_1bef <- df_clean_postSC_1bef %>% filter(City == "Waimakariri District")
df_WMK_preRAW_1bef <- df_clean_preRAW_1bef %>% filter(City == "Waimakariri District")
df_WMK_postRAW_1bef <- df_clean_postRAW_1bef %>% filter(City == "Waimakariri District")

# MSPE:
df_WMK_pre_1bef <- merge(df_WMK_preRAW_1bef, df_WMK_preSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_WMK_post_1bef <- merge(df_WMK_postRAW_1bef, df_WMK_postSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_WMK_pre_1bef <- mean((df_WMK_pre_1bef$demeaned_outcome - df_WMK_pre_1bef$sc_value_1bef)^2)
mspe_WMK_post_1bef <- mean((df_WMK_post_1bef$demeaned_outcome - df_WMK_post_1bef$sc_value_1bef)^2)

# Print the result
print(mspe_WMK_pre_1bef)
print(mspe_WMK_post_1bef)

# MSPE-R
MSPER_WMK_1bef <- (mspe_WMK_post_1bef / mspe_WMK_pre_1bef)
print(MSPER_WMK_1bef)

###################### SELWYN
df_SLW_preSC_1bef <- df_clean_preSC_1bef %>% filter(City == "Selwyn District")
df_SLW_postSC_1bef <- df_clean_postSC_1bef %>% filter(City == "Selwyn District")
df_SLW_preRAW_1bef <- df_clean_preRAW_1bef %>% filter(City == "Selwyn District")
df_SLW_postRAW_1bef <- df_clean_postRAW_1bef %>% filter(City == "Selwyn District")

# MSPE:
df_SLW_pre_1bef <- merge(df_SLW_preRAW_1bef, df_SLW_preSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_SLW_post_1bef <- merge(df_SLW_postRAW_1bef, df_SLW_postSC_1bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_SLW_pre_1bef <- mean((df_SLW_pre_1bef$demeaned_outcome - df_SLW_pre_1bef$sc_value_1bef)^2)
mspe_SLW_post_1bef <- mean((df_SLW_post_1bef$demeaned_outcome - df_SLW_post_1bef$sc_value_1bef)^2)

# Print the result
print(mspe_SLW_pre_1bef)
print(mspe_SLW_post_1bef)

# MSPE-R
MSPER_SLW_1bef <- (mspe_SLW_post_1bef / mspe_SLW_pre_1bef)
print(MSPER_SLW_1bef)



## Two periods before:

combined_data_2bef <- combined_data %>%
  mutate(treatment = case_when(
    TA == "Auckland" & Year >= 2011 ~ 1,
    TA == "Lower Hutt City" & Year >= 2016 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2010 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District") %>%
  filter(TA != "Hurunui District") %>%
  filter(TA != "Ashburton District") 

all_combined_data_demeaned_2bef <- combined_data_2bef %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

scm_all_2bef <- scdataMulti(all_combined_data_demeaned_2bef, id.var = "TA", outcome.var = "demeaned_outcome",
                       treatment.var = "treatment", time.var = "Year", constant = TRUE,
                       cointegrated.data = TRUE, features = list("demeaned_outcome"))
### SIMPLEX
res_simplex_ALL_2bef <- scest(scm_all_2bef, w.constr = list("name" = "simplex"))

# Synthetic control values pre-treatment
df_preSC_2bef <- as.data.frame(res_simplex_ALL_2bef$est.results$Y.pre.fit)
df_clean_preSC_2bef <- df_preSC_2bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value_2bef = V1) # Rename V1 to sc_value_2bef

# Synthetic control values post-treatment
df_postSC_2bef <- as.data.frame(res_simplex_ALL_2bef$est.results$Y.post.fit)
df_clean_postSC_2bef <- df_postSC_2bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) %>% # Convert Year to numeric
  rename(sc_value_2bef = V1) # Rename V1 to sc_value_2bef

# Actual values pre-treatment
df_preRAW_2bef <- as.data.frame(res_simplex_ALL_2bef$data$Y.pre)
df_clean_preRAW_2bef <- df_preRAW_2bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) # Convert Year to numeric

# Actual values post-treatment
df_postRAW_2bef <- as.data.frame(res_simplex_ALL_2bef$data$Y.post)
df_clean_postRAW_2bef <- df_postRAW_2bef %>%
  rownames_to_column(var = "Location") %>%
  separate(Location, into = c("City", "Year"), sep = "\\.") %>%
  mutate(Year = as.numeric(Year)) # Convert Year to numeric

###################### AUCKLAND
# Filter all datasets to just have Auckland data
df_akl_preSC_2bef <- df_clean_preSC_2bef %>% filter(City == "Auckland")
df_akl_postSC_2bef <- df_clean_postSC_2bef %>% filter(City == "Auckland")
df_akl_preRAW_2bef <- df_clean_preRAW_2bef %>% filter(City == "Auckland")
df_akl_postRAW_2bef <- df_clean_postRAW_2bef %>% filter(City == "Auckland")

# MSPE:
df_akl_pre_2bef <- merge(df_akl_preRAW_2bef, df_akl_preSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_akl_post_2bef <- merge(df_akl_postRAW_2bef, df_akl_postSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_akl_pre_2bef <- mean((df_akl_pre_2bef$demeaned_outcome - df_akl_pre_2bef$sc_value_2bef)^2)
mspe_akl_post_2bef <- mean((df_akl_post_2bef$demeaned_outcome - df_akl_post_2bef$sc_value_2bef)^2)

# Print the result
print(mspe_akl_pre_2bef)
print(mspe_akl_post_2bef)

# MSPE-R
MSPER_akl_2bef <- (mspe_akl_post_2bef / mspe_akl_pre_2bef)
print(MSPER_akl_2bef)


###################### LOWER HUTT
df_LH_preSC_2bef <- df_clean_preSC_2bef %>% filter(City == "Lower Hutt City")
df_LH_postSC_2bef <- df_clean_postSC_2bef %>% filter(City == "Lower Hutt City")
df_LH_preRAW_2bef <- df_clean_preRAW_2bef %>% filter(City == "Lower Hutt City")
df_LH_postRAW_2bef <- df_clean_postRAW_2bef %>% filter(City == "Lower Hutt City")

# MSPE:
df_LH_pre_2bef <- merge(df_LH_preRAW_2bef, df_LH_preSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_LH_post_2bef <- merge(df_LH_postRAW_2bef, df_LH_postSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_LH_pre_2bef <- mean((df_LH_pre_2bef$demeaned_outcome - df_LH_pre_2bef$sc_value_2bef)^2)
mspe_LH_post_2bef <- mean((df_LH_post_2bef$demeaned_outcome - df_LH_post_2bef$sc_value_2bef)^2)

# Print the result
print(mspe_LH_pre_2bef)
print(mspe_LH_post_2bef)

# MSPE-R
MSPER_LH_2bef <- (mspe_LH_post_2bef / mspe_LH_pre_2bef)
print(MSPER_LH_2bef)

###################### CHRISTCHURCH
df_CHCH_preSC_2bef <- df_clean_preSC_2bef %>% filter(City == "Christchurch City")
df_CHCH_postSC_2bef <- df_clean_postSC_2bef %>% filter(City == "Christchurch City")
df_CHCH_preRAW_2bef <- df_clean_preRAW_2bef %>% filter(City == "Christchurch City")
df_CHCH_postRAW_2bef <- df_clean_postRAW_2bef %>% filter(City == "Christchurch City")

# MSPE:
df_CHCH_pre_2bef <- merge(df_CHCH_preRAW_2bef, df_CHCH_preSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_CHCH_post_2bef <- merge(df_CHCH_postRAW_2bef, df_CHCH_postSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_CHCH_pre_2bef <- mean((df_CHCH_pre_2bef$demeaned_outcome - df_CHCH_pre_2bef$sc_value_2bef)^2)
mspe_CHCH_post_2bef <- mean((df_CHCH_post_2bef$demeaned_outcome - df_CHCH_post_2bef$sc_value_2bef)^2)

# Print the result
print(mspe_CHCH_pre_2bef)
print(mspe_CHCH_post_2bef)

# MSPE-R
MSPER_CHCH_2bef <- (mspe_CHCH_post_2bef / mspe_CHCH_pre_2bef)
print(MSPER_CHCH_2bef)

###################### WAIMAKARIRI
df_WMK_preSC_2bef <- df_clean_preSC_2bef %>% filter(City == "Waimakariri District")
df_WMK_postSC_2bef <- df_clean_postSC_2bef %>% filter(City == "Waimakariri District")
df_WMK_preRAW_2bef <- df_clean_preRAW_2bef %>% filter(City == "Waimakariri District")
df_WMK_postRAW_2bef <- df_clean_postRAW_2bef %>% filter(City == "Waimakariri District")

# MSPE:
df_WMK_pre_2bef <- merge(df_WMK_preRAW_2bef, df_WMK_preSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_WMK_post_2bef <- merge(df_WMK_postRAW_2bef, df_WMK_postSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_WMK_pre_2bef <- mean((df_WMK_pre_2bef$demeaned_outcome - df_WMK_pre_2bef$sc_value_2bef)^2)
mspe_WMK_post_2bef <- mean((df_WMK_post_2bef$demeaned_outcome - df_WMK_post_2bef$sc_value_2bef)^2)

# Print the result
print(mspe_WMK_pre_2bef)
print(mspe_WMK_post_2bef)

# MSPE-R
MSPER_WMK_2bef <- (mspe_WMK_post_2bef / mspe_WMK_pre_2bef)
print(MSPER_WMK_2bef)

###################### SELWYN
df_SLW_preSC_2bef <- df_clean_preSC_2bef %>% filter(City == "Selwyn District")
df_SLW_postSC_2bef <- df_clean_postSC_2bef %>% filter(City == "Selwyn District")
df_SLW_preRAW_2bef <- df_clean_preRAW_2bef %>% filter(City == "Selwyn District")
df_SLW_postRAW_2bef <- df_clean_postRAW_2bef %>% filter(City == "Selwyn District")

# MSPE:
df_SLW_pre_2bef <- merge(df_SLW_preRAW_2bef, df_SLW_preSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))
df_SLW_post_2bef <- merge(df_SLW_postRAW_2bef, df_SLW_postSC_2bef, by = "Year", suffixes = c("_raw", "_predicted"))

# Calculate MSPE
mspe_SLW_pre_2bef <- mean((df_SLW_pre_2bef$demeaned_outcome - df_SLW_pre_2bef$sc_value_2bef)^2)
mspe_SLW_post_2bef <- mean((df_SLW_post_2bef$demeaned_outcome - df_SLW_post_2bef$sc_value_2bef)^2)

# Print the result
print(mspe_SLW_pre_2bef)
print(mspe_SLW_post_2bef)

# MSPE-R
MSPER_SLW_2bef <- (mspe_SLW_post_2bef / mspe_SLW_pre_2bef)
print(MSPER_SLW_2bef)

#Inference 2:

#OG model
df_.k_all <- scdataMulti(all_combined_data_demeaned, id.var = "TA", outcome.var = "demeaned_outcome",
                         treatment.var = "treatment", time.var = "Year", constant = TRUE,
                         cointegrated.data = FALSE, features = list("demeaned_outcome"),
                         effect = "time", verbose = TRUE)
### SIMPLEX
res_SIMPLEX_.k_all <- scest(df_.k_all, w.constr = list("name" = "simplex"))
a <- scplotMulti(res_SIMPLEX_.k_all)

#1 period before
combined_data_1bef <- combined_data %>%
  mutate(treatment = case_when(
    TA == "Auckland" & Year >= 2012 ~ 1,
    TA == "Lower Hutt City" & Year >= 2017 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2011 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District") %>%
  filter(TA != "Hurunui District") %>%
  filter(TA != "Ashburton District") 

all_combined_data_demeaned_1bef <- combined_data_1bef %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

scm_all_1bef <- scdataMulti(all_combined_data_demeaned_1bef, id.var = "TA", outcome.var = "demeaned_outcome",
                            treatment.var = "treatment", time.var = "Year", constant = TRUE,
                            cointegrated.data = TRUE, effect = "time", features = list("demeaned_outcome"))
### SIMPLEX
res_simplex_ALL_1bef <- scest(scm_all_1bef, w.constr = list("name" = "simplex"))
b <- scplotMulti(res_simplex_ALL_1bef)

#2 periods before
combined_data_2bef <- combined_data %>%
  mutate(treatment = case_when(
    TA == "Auckland" & Year >= 2011 ~ 1,
    TA == "Lower Hutt City" & Year >= 2016 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2010 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Kaipara District") %>%
  filter(TA != "Waikato District") %>%
  filter(TA != "Hurunui District") %>%
  filter(TA != "Ashburton District") 

all_combined_data_demeaned_2bef <- combined_data_2bef %>%
  group_by(TA) %>%
  mutate(
    treatment_year = min(Year[treatment == 1]),
    pre_treatment_mean = mean(consents[Year < treatment_year], na.rm = TRUE),
    demeaned_outcome = consents - pre_treatment_mean
  ) %>%
  ungroup()

scm_all_2bef <- scdataMulti(all_combined_data_demeaned_2bef, id.var = "TA", outcome.var = "demeaned_outcome",
                            treatment.var = "treatment", time.var = "Year", constant = TRUE,
                            cointegrated.data = TRUE, effect = "time", features = list("demeaned_outcome"))

res_simplex_ALL_2bef <- scest(scm_all_2bef, w.constr = list("name" = "simplex"))
c <- scplotMulti(res_simplex_ALL_2bef)




