# packages used
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)

# Load in base data set

combined_data <- read_excel("combined_data.xlsx")

## initial synthetic control looking just at Lower Hutt
library(augsynth)

#filtering out all TAs that experienced zoning reform, and potential spillover areas (bordering TAs)
LH_combined_data <- combined_data %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Auckland") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Christchurch City") %>%
  filter(TA != "Waimakariri District") %>%
  filter(TA != "Selwyn District")

#single treated unit (LH) example
syn <- augsynth(consents ~ zoning_reform, TA, Year, LH_combined_data,
                progfunc = "None", scm = T)
summary(syn)
plot(syn)

#multiple treated units (CHCH + SLW + WMK) example
CHCH_combined_data <- combined_data %>%
  filter(TA != "New Zealand") %>%
  filter(TA != "Auckland") %>%
  filter(TA != "Upper Hutt City") %>%
  filter(TA != "Wellington City") %>%
  filter(TA != "Porirua City") %>%
  filter(TA != "South Wairarapa District") %>%
  filter(TA != "Lower Hutt City")

syn_mlt <- multisynth(consents ~ zoning_reform, TA, Year, CHCH_combined_data)
print(syn_mlt$nu)
plot(summary(syn_mlt))
summary(syn_mlt)
plot(syn_mlt)

# Inference
library(scpi)

combined_data$treatment <- 0
combined_data[(combined_data$TA == "Lower Hutt" & combined_data$Year >= 1991), 'treatment'] <- 1
combined_data[(combined_data$TA == "Auckland" & combined_data$Year >= 2016), 'treatment'] <- 1

data$treatment <- 0
data[(data$country == "West Germany" & data$year >= 1991), 'treatment'] <- 1
data[(data$country == "Italy" & data$year >= 1992), 'treatment'] <- 1












