# packages used
library(readxl)
library(writexl)
library(tidyr)
library(dplyr)
library(ggplot2)

# Read the building consents files
file_path <- "Data/Ready/buildingconsents_original.xlsx"
df <- read_excel(file_path, sheet = "matrix")

# Reshape the data into long format
df_long <- pivot_longer(df, cols = -Year, 
                        names_to = "TA", values_to = "consents") %>%
  mutate(Year = as.integer(Year))

# Display the reshaped data frame
head(df_long)

### Adding in Matching Variables

## MV1: dwellings per capita

# Load the dwellings per capita data
dwellings_per_capita <- read_excel("Data/Ready/dwellings_percapita.xlsx")

dwelling_per_capita_long <- dwellings_per_capita %>%
  pivot_longer(cols = c("2006", "2013", "2018"),
               names_to = "Year",
               values_to = "Dwelling_Per_Capita") %>%
  mutate(Year = as.integer(Year))

## MV2: Median HH Income % inc 2006-2013
median_hh_income_change <- read_excel("Data/Ready/median hh income % inc 2006-2013.xlsx")

income_long <- median_hh_income_change %>%
  pivot_longer(cols = "2013",
               names_to = "Year",
               values_to = "Household Income Change") %>%
  mutate(Year = as.integer(Year))

## MV3: Projected Population
projected_population <- read_excel("Data/Ready/Subnational-population projections 2013 + 2018 base (for next 30 years)e.xlsx")

projected_population_long <- projected_population %>%
  pivot_longer(cols = c("2013","2018"),
               names_to = "Year",
               values_to = "Projected Population Growth") %>%
  mutate(Year = as.integer(Year))

## MV4: Actual Population Growth
population_growth <- read_excel("Data/Ready/Usually resident population difference of logs.xlsx")

population_growth_long <- population_growth %>%
  pivot_longer(cols = c("2006", "2013", "2018"),
               names_to = "Year",
               values_to = "Actual Population Growth") %>%
  mutate(Year = as.integer(Year))


### Combining Data sets

combined_data <- df_long %>%
  left_join(dwelling_per_capita_long, 
            by = c("TA", "Year")) %>%
  left_join(income_long, 
            by = c("TA", "Year")) %>%
  left_join(projected_population_long, 
            by = c("TA", "Year")) %>%
  left_join(population_growth_long, 
            by = c("TA", "Year"))



# create treatment status column that indicates which TA is treated in a given year
# I also remove New Zealand (included initially for summary statistics)

combined_data <- combined_data %>%
  mutate(zoning_reform = case_when(
    TA == "Auckland" & Year >= 2013 ~ 1,
    TA == "Lower Hutt City" & Year >= 2020 ~ 1,
    TA %in% c("Christchurch City", "Selwyn District", "Waimakariri District") & Year >= 2013 ~ 1,
    TRUE ~ 0)) %>%
  filter(TA != "New Zealand") %>%
  mutate(TA = as.factor(TA),
         Year = as.numeric(Year))

write_xlsx(combined_data, "combined_data.xlsx")


### De-meaning the outcome variable





