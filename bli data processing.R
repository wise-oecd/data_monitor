# Data wrangling packages
library(tidymodels)
library(tidyverse)
library(janitor)
library(purrr)
# library(stringr)

# Data download packages
library(rsdmx)
library(eurostat)
library(pdftools)
library(xml2)
library(writexl)
library(readxl)

# Plot packages
library(oecdplot)
library(patchwork)
library(ggrepel)

# Statistical packages
library(parsnip)
library(metrica)
library(Hmisc)

source("./global_processing.R")
options(scipen=999)


# Discussion points -------------------------------------------------------

# GENERAL:
# 1. The default region should not be set as first in list (4_3, 7_2)
#    Ex: LAC can't be predicted for 7_2 because no training data for the continent 
#    Currently methodology uses Africa to predict values (depends on val set model_continent_levels[1])
#    Predicted values therefore sensitive to this!
# 2. Naming conventions: Northern America wasn't catching appropriately in scatters (North America)
#    Some countries were missed due to oecdcountrycode for 3_1 (GBR + GRC) VERY small impact
# 3. Add in AUC/ROC to check impact of removing outliers?


# INDICATOR-SPECIFC:
# 1_1 HADI: CHE was removed from db, pulled in previous data temporarily
#           Removing IRL + LUX improves model notably, just curious your logic when including or not
#
# 1_3 Household wealth: Regional model better than baseline + regional best w/o DNK
#
# 4_1 Leisure time: ITA + AUT removed is a better fit now
#
# 4_3 + 7_2: Fix LAC region attributions 



# Tidying -----------------------------------------------------------------

# Toggle to no when testing
versioning <- "yes"

mainpath <- "S:/Data/WDP/Well being database/Data Monitor/data_monitor/"

# Set filter for indicators in BLI
headline_indics <- c(material_headline, quality_headline, community_headline)

headline_indics[headline_indics == "10_2_DEP"] <- "10_2_GAP"

# Full country average data 
full_dat <- readRDS("//main.oecd.org/sdataWIS/Data/WDP/Well being database/Automated database/output/final dataset.RDS") %>%
  mutate(time_period = as.numeric(time_period)) %>%
  filter(!ref_area == "OECD")

# -------------------------------------------------------------------------

# Calculation of gender gap in 10_2 
indic_gap_10_2 <- full_dat %>%
  filter(measure == "10_2", sex %in% c("M", "F")) %>%
  select(ref_area, time_period, measure, sex, obs_value) %>%
  pivot_wider(names_from = "sex", values_from = "obs_value") %>%
  mutate(obs_value = `F` - `M`,
         measure = "10_2_GAP",
         obs_status = "A") %>%
  select(-`F`, -`M`) %>%
  drop_na(obs_value) %>% 
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() 


bli_dat <- full_dat %>% 
  filter(sex == "_T", age == "_T", education_lev == "_T",
         !measure == "10_2") %>%
  select(ref_area, time_period, measure, obs_value, obs_status) %>%
  rbind(indic_gap_10_2) %>%
  filter(measure %in% headline_indics) %>%
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup()

rm(indic_gap_10_2)


# DATA REPLACEMENT  -------------------------------------------------------


# 3_1 Overcrowding rate ---------------------------------------------------
# Note (14/10/25): to include 2016 value for Canada (this can be removed when value included in the database)
# This can be removed if Canada is ever updated for overcrowding rate

# Change #5: Made the data pull traceable. This is important for reproducability and data validation 
# For example, I  was unsure if 0.60 should have been multiplied by 100, can check easily now it's connected to source
# rather than entered directly. It can also reduce errors from data entry. 

missing_overcrowding <- bli_dat %>% 
  filter(measure == "3_1", ref_area %in% oecd_countries) %>% 
  pull(ref_area) %>% 
  setdiff(oecd_countries, .)

# Pull data for Canada from other sheet in Affordable Housing Database and bind
indic_3_1 <- openxlsx::read.xlsx("//main.oecd.org/sdataWIS/Data/WDP/Well being database/Automated database/preprocessing/3_1 Overcrowding rate.xlsx", sheet = "HC2.1.3") 

# Use REGEX to pull out the latest year for Canada   
missing_year <- indic_3_1 %>%
  slice(26) %>%
  select(time_period = 1) %>%
  separate_rows(time_period,  sep = ";") %>%
  separate_rows(time_period, sep = "\\.") %>%
  separate(time_period, into=c("ref_area", "time_period"), sep = "to ") %>%
  group_by(time_period) %>%
  separate_rows(ref_area, sep = ",") %>%
  separate_rows(ref_area, sep = " and ") %>%
  ungroup() %>%
  mutate(ref_area = countrycode::countrycode(ref_area, "country.name", "iso3c")) %>%
  drop_na() %>%
  filter(ref_area %in% missing_overcrowding) 

bli_dat <- indic_3_1 %>%
  .[-c(1:2),] %>%
  select(ref_area = 2, obs_value = 6) %>%
  mutate(ref_area = countrycode::countrycode(ref_area, "country.name", "iso3c")) %>%
  drop_na() %>% 
  filter(ref_area %in% missing_overcrowding) %>%
  mutate(measure = "3_1",
         obs_status = "E")  %>%
  merge(missing_year) %>%
  rbind(bli_dat) 

# Verify it was added
bli_dat %>%
  filter(measure == "3_1", ref_area == "CAN") %>%
  arrange(desc(time_period))

rm(missing_year, missing_overcrowding, indic_3_1)

# 9_2 Air pollution -------------------------------------------------------
# Note: Add previous version 9_2 data for KOR - TO BE REMOVED when data for KOR available
# KOR still unavailable as of March 2026 update

# Pull from latest database version with KOR data
bli_dat <- readRDS("S:/Data/WDP/Well being database/Automated database/output/versioning/final dataset_2025-05-06.RDS") %>%
  mutate(time_period = as.numeric(time_period)) %>%
  filter(ref_area == "KOR", measure == "9_2") %>%
  filter(time_period == max(time_period)) %>%
  select(ref_area, time_period, measure, obs_value) %>%
  mutate(obs_status = "E") %>%
  rbind(bli_dat) %>%
  arrange(measure, ref_area, time_period)

# Verify it was added
bli_dat %>%
  filter(measure == "9_2", ref_area == "KOR") %>%
  arrange(desc(time_period))

# Create complete list ----------------------------------------------------

# Pull out complete indicators for filtering
oecd_completes <- bli_dat %>%
  filter(ref_area %in% oecd_countries, measure %in% headline_indics) %>%
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  count(measure) %>%
  filter(n == length(oecd_countries)) %>%
  pull(measure) %>%
  sort()

# Load previous list of complete indicators
oecd_completes_previous <- readRDS(paste0(mainpath, "/data/complete_indicators.RDS")) %>% sort()

# Check if current and previous filter are the same
complete_check <- all(oecd_completes == oecd_completes_previous)

if(complete_check == F) {
  stop("New and previous complete lists do not match: new indicators now complete or new indicator incomplete")
  # saveRDS(oecd_completes, "./complete_indicators.RDS")
}

hsl_dat <- bli_dat %>% 
  filter(!measure %in% oecd_completes) %>%
  mutate(obs_value = as.numeric(obs_value))

rm(complete_check, oecd_completes_previous, oecd_completes)



# IMPUTATION --------------------------------------------------------------


# 1_1 HADI ----------------------------------------------------------------
# Consistent - predictor: WB GINI linear model

# Questions: ----
# We keep outliers in the end?
# Why add in the complete factor function? (default level to Asia in imputator)
# No difference between ISR + TUR in South Europe ? 
# --------------

# Find most common year for 1_1 in db to impute missing data with
most_common_year_indic <- hsl_dat %>%
  filter(measure == "1_1") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

# Pull all countries available & find those to be imputed
countries_avail <- hsl_dat %>% filter(measure == "1_1") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]

# Pull non-OECD countries that can be used to train model
partner_avail <- setdiff(oecd_countries, countries_avail)

# Use WDI API to pull 
gni_dat <- WDI::WDI(country = c(oecd_countries, partner_avail),
                    indicator = "NY.GNP.PCAP.PP.CD", 
                    start=most_common_year_indic, 
                    end=most_common_year_indic) %>%
  select(ref_area = iso3c, replace_indic = "NY.GNP.PCAP.PP.CD") %>%
  mutate(replace_indic = as.numeric(replace_indic))


# Scatter plot analysis to identify possible outliers and remove them from the imputation process
# CHECK that outliers are based on evidence (e.g. LUX, IRL, NOR)
# Adding regions (not continent) and their interactions significantly improve fit
scatterplotCompare(hsl_dat, gni_dat, "1_1")
scatterplotCompare(hsl_dat, gni_dat %>% filter(!ref_area %in% c("IRL", "LUX")), "1_1")

# Imputation with regions and interaction perform better than baseline without moderate outliers 
# With TUR and ISR included in south Europe
hadi_imputed <- imputatorFunction(hsl_dat, gni_dat, 
                                  "1_1", 
                                  imputing_iso3c, 
                                  model_type = "region_inter",
                                  south_europe_additions = T)


# Include obs_status/flag for estimate
indic_1_1 <- hadi_imputed$indic_dat %>% 
  mutate(time_period = most_common_year_indic) %>% 
  mutate(obs_status= "E") %>%
  rbind(hsl_dat %>% filter(measure == "1_1"))

rm(gni_dat, most_common_year_indic, countries_avail, partner_avail, 
   imputing_iso3c, hadi_imputed)

# 1_2 S20/80 income share -------------------------------------------------
# New 2025 - predictor: S80/S20 from UN WIDER WIID - source LISSY)
# Predictor: S80S20 square root equivalised household net income from UN WIDER WIID 
# (S80S20, Gini and Palma plus other deciles available + various income definitions and transformations + multiple sources)
# perfect correlation and all countries available (correlation with GINI strong, but not perfect) - 
# Just one predictor to avoid collinearity/redundancy with others
# priority given to data from LIS through LISSY for coherence 

# Download from : https://www.wider.unu.edu/database/world-income-inequality-database-wiid

# Check countries for which data are available and those missing
countries_avail <- hsl_dat %>% filter(measure == "1_2") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]

# Step 1: Read data and drop the 1st and 4th columns
wiid_raw <- openxlsx::read.xlsx("https://www.wider.unu.edu/sites/default/files/WIID/WIID-29APR2025.xlsx")

lissy_sources <- c(
  "Own construction based on LIS Database through LISSY",
  "Own construction based on ERF/LIS Database through LISSY"
)

wiid_short <- wiid_raw |> 
  select(-1, -4) |> 
  pivot_longer(gini:top5, names_to = "measure", values_to = "obs_value") |> 
  filter(
    measure %in% c("gini", "ratio_top20bottom20", "palma"),
    resource == "Income (net)",
    scale_detailed == "Square root",
    sharing_unit == "Household",
    reference_unit == "Person",
    year >= 2004,
    c3 %in% c(oecd_countries, partner_countries)
  ) |> 
  # !!!! ADDITION !!!!
  # Check order here
  mutate(
    priority = case_when(
      source_comments %in% lissy_sources ~ 1,
      revision == "New 2025"             ~ 2,
      revision == "New 2023"             ~ 3,
      TRUE ~ 4)
  ) |> 
  group_by(ref_area = c3, measure) |> 
  slice_min(order_by = priority, n = 1, with_ties = TRUE) |>
  filter(year == max(year)) |> 
  ungroup() |> 
  select(ref_area, measure, obs_value, time_period = year)


# b) ANALYSE DATA TO CHOOSE PREDICTOR/S - not necessary when you just need the imputations 

# Append the two to check correlation  across indicators
s82_short <- hsl_dat %>% 
  filter(measure == "1_2") %>%
  select(ref_area, measure, obs_value, time_period) 

# 1. Pivot back to wide format
s82_matrix <- rbind(s82_short, wiid_short) %>%
  pivot_wider(names_from = measure, values_from = obs_value) %>%
  select(-ref_area, -time_period) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  as.matrix()

# 3. Compute correlations with p-values
corr_res <- Hmisc::rcorr(s82_matrix)

# 4. Extract correlation coefficients and p-values
corr_coef <- corr_res$r        # correlation coefficients
corr_pval <- corr_res$P        # p-values

# 5. Create a significance table (e.g., stars: * p<0.05, ** p<0.01, *** p<0.001)
sig_stars <- matrix("", nrow=nrow(corr_pval), ncol=ncol(corr_pval))
sig_stars[corr_pval < 0.05] <- "*"
sig_stars[corr_pval < 0.01] <- "**"
sig_stars[corr_pval < 0.001] <- "***"

# 6. Combine coefficients and significance symbols into a readable table
corr_table_with_sig <- matrix(
  paste0(round(corr_coef, 2), sig_stars),
  nrow = nrow(corr_coef),
  ncol = ncol(corr_coef),
  dimnames = list(rownames(corr_coef), colnames(corr_coef))
)

# 7. Inspect the final table
corr_table_with_sig

# Step 2. ANALYSIS AND IMPUTATION WITH SELECTED PREDICTOR 

wiid_s82 <- wiid_short %>% 
  filter(measure == "ratio_top20bottom20") %>%
  select(ref_area, replace_indic = obs_value)

# Scatter plot analysis to identify possible outliers and remove them from the imputation process
scatterplotCompare(hsl_dat, wiid_s82, "1_2")
# GBR is an extreme outlier and the fit slightly improve without it, 
# but the model fit is good with GBR and there is no justification to exclude GBR
scatterplotCompare(hsl_dat, wiid_s82 %>% filter(!ref_area == "GBR"), "1_2")

# Run imputation function
s82_imputed <- imputatorFunction(hsl_dat, wiid_s82, "1_2", imputing_iso3c, model_type = "baseline")

# Merge imputed data with time period
indic_1_2 <- wiid_short %>% 
  filter(measure == "ratio_top20bottom20") %>%
  select(ref_area, time_period) %>%
  merge(s82_imputed$indic_dat) %>%
  mutate(obs_status = "E") %>%
  rbind(hsl_dat %>% filter(measure == "1_2"))

rm(s82_imputed, wiid_s82,  wiid_short, wiid_raw, imputing_iso3c,
   lissy_sources, corr_coef, corr_pval, corr_res, corr_table_with_sig, 
   s82_matrix, s82_short, sig_stars)

# 1_3 Household net wealth -----------------------------------------------
# Source: Credit Suisse Median wealth per adult (USD) latest available year: 2022
# NOTE: ANNUALLY CHECK FOR UPDATED VERSIONS OF THE CREDIT SUISSE GLOBAL WEALTH DATABOOK
# Estimated with Credit Suisse wealth: https://rev01ution.red/wp-content/uploads/2024/03/global-wealth-databook-2023-ubs.pdf 
# Tested also  with most common year (2021) and with mean wealth per adult (2022)
# Correlation with median wealth per adult (0.78> 0.86 without DNK) higher than with 2022 mean wealth per adult, with median wealth per adult 2021 (0.797)
# Lower correlation with WB GDP per capita (0.46> 0.67 without USA), WB Households and NPISHs Final consumption expenditure, PPP (constant 2021 international $) (-0.06)

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "1_3") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# 9 countries: "CHE" "COL" "CRI" "ISL" "ISR" "JPN" "MEX" "SWE" "TUR"

#Extract median wealth per adult data from the Credit Suisse global wealth databook 2023 pdf

pdf_text_all <- pdf_text("https://rev01ution.red/wp-content/uploads/2024/03/global-wealth-databook-2023-ubs.pdf")

cs_wealth <- unlist(lapply(123:126, function(p) { str_split(pdf_text_all[p], "\n")[[1]] })) %>%
  # Step 1: trim lines and remove empty
  str_trim() %>%
  purrr::discard(~ .x == "" | str_detect(.x, "Market|USD")) |>
  # Step 2: extract Country, Mean wealth, Median wealth
  str_match("^([A-Za-z\\s\\.\\-]+?)\\s+\\d[0-9,]*\\s+([0-9,]+)\\s+([0-9,]+)") %>%
  .[,2:4] %>%
  as_tibble() %>%
  rename("market" = 1, "mean_wealth" = 2, "median_wealth" = 3) %>%
  # Step 3: remove commas and convert to numeric
  mutate(
    mean_wealth = as.numeric(str_replace_all(mean_wealth, ",", "")),
    median_wealth = as.numeric(str_replace_all(median_wealth, ",", "")),
    #Step 4 - ISO3 code instead of country names
    ref_area = oecdcountrycode::oecdcountrycode(market, "country.name","iso3c")
  ) %>%
  # Step 5 - List excluded markets (those without an ISO3 code)
  drop_na(ref_area) %>%
  select(ref_area, mean_wealth, median_wealth)


# --- Correlation with wealth_short$obs_value ---
merged_data <- cs_wealth %>%
  inner_join(hsl_dat %>% filter(measure == "1_3", ref_area %in% all_countries) %>% select(ref_area, obs_value), by = "ref_area")

cor_mean <- cor(merged_data$mean_wealth, merged_data$obs_value, use = "complete.obs")
cor_median <- cor(merged_data$median_wealth, merged_data$obs_value, use = "complete.obs")

cat("Correlation of mean wealth vs 1_3:", round(cor_mean, 3), "\n")
cat("Correlation of median wealth vs 1_3:", round(cor_median, 3), "\n")

cs_wealth_short <- cs_wealth %>% select(ref_area, replace_indic = median_wealth)

# Adding continent or regions and their interactions do NOT significantly improve fit 
scatterplotCompare(hsl_dat, cs_wealth_short,"1_3")
# Best fit removing only DNK
scatterplotCompare(hsl_dat, cs_wealth_short %>% filter(!ref_area == "DNK"), "1_3")

wealth_imputed <- imputatorFunction(hsl_dat, cs_wealth_short %>% filter(!ref_area == "DNK"), "1_3", imputing_iso3c, "baseline") 

#Imputed values (revise year of reference as needed)
indic_1_3 <- wealth_imputed$indic_dat %>% 
  mutate(time_period= "2022") %>% 
  mutate(obs_status= "E") %>%
  rbind(hsl_dat %>% filter(measure == "1_3"))

rm(merged_data, cor_mean, cor_median, cs_wealth_short, cs_wealth,
   wealth_imputed, cs_wealth, pdf_text_all, countries_avail, imputing_countries)


# 2_7 Long hours in paid work ---------------------------------------------
# UPDATED from direct replacement/proxy: share of employed (employees and self-employed) workers usually 
# working long hours (50 and more)) to regression with regional factors as improve fit
# Calculating the % from Total employed from OECD Long usual weekly working hours database
# Latest year considered as indicators are aligned in terms of latest available year

# Countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "2_7") %>% drop_na() %>% pull(ref_area) 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# Two countries: "JPN" "KOR"

long_hours_data <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_HW@DF_LNG_WK_HRS/", paste0(oecd_countries, collapse = "+"),"..._T._T...._T...H1T49+H_GE50.?startPeriod=2004&dimensionAtObservation=AllDimensions") %>%
  rsdmx::readSDMX() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  select(ref_area, time_period, sex, age, hour_bands, obs_value, obs_status) %>%
  pivot_wider(names_from="hour_bands", values_from="obs_value") %>%
  mutate(tot = H1T49 + H_GE50,
         obs_value = (H_GE50/tot)*100,
         obs_value = round(obs_value, 2),
         time_period = as.numeric(time_period)) %>%
  select(-H1T49, -H_GE50, -tot) %>%
  group_by(ref_area) %>% 
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(ref_area, replace_indic= obs_value, time_period)


# Including the regional factor improve the already good fit (slightly more than the continent factor)
scatterplotCompare(hsl_dat, long_hours_data, "2_7")
scatterplotCompare(hsl_dat, long_hours_data %>% filter(!ref_area == "GRC"), "2_7")

longhr_imputed <- imputatorFunction(hsl_dat, long_hours_data, "2_7", imputing_iso3c, model_type = "region_group")

indic_2_7 <- long_hours_data %>%
  select(ref_area, time_period) %>%
  merge(longhr_imputed$indic_dat) %>%
  mutate(obs_status= "E") %>%
  rbind(hsl_dat %>% filter(measure == "2_7"))

rm(countries_avail, imputing_iso3c, long_hours_data, longhr_imputed)


# 3_1 Overcrowding rate --------------------------------------------------
# NEW 2025: Rooms per person from Eurostat with calculations from ABS (bedrooms) 
# and ISR CBS (from housing density)
# Estimated with rooms per person 

countries_avail <- hsl_dat %>% filter(measure == "3_1") %>% drop_na() %>% pull(ref_area) 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
#2: AUS and ISR missing (2016 value for CAN used as of March 2026)

# AUS ABS Housing Occupancy and Costs, calculation of bedrooms per person 
# from the press release: https://www.abs.gov.au/statistics/people/housing/housing-occupancy-and-costs/latest-release
aus_vals <- openxlsx::read.xlsx("https://www.abs.gov.au/statistics/people/housing/housing-occupancy-and-costs/2019-20/1.%20Housing%20occupancy%20and%20costs%2C%20Australia%2C%201994%E2%80%9395%20to%202019%E2%80%9320.xlsx", sheet = "Table 1.3", startRow = 5, colNames = T) %>%
  rename(measure = 1, unit = 2) %>%
  filter(measure %in% c("Mean number of persons in household", "Mean number of bedrooms in dwelling"),
         !grepl("RSE", unit)) %>%
  pivot_longer(!c(measure, unit), names_to = "time_period", values_to = "obs_value",
               names_transform = list(time_period = parse_number)) %>%
  filter(time_period == max(time_period)) %>%
  select(-unit) %>%
  mutate(
    measure = case_when(
      grepl("household", measure) ~ "house",
      TRUE ~ "dwelling"
    )
  ) %>%
  pivot_wider(names_from = "measure", values_from = "obs_value") %>%
  mutate(replace_indic = dwelling / house,
         ref_area = "AUS") %>%
  select(ref_area, time_period, replace_indic)


# Download data from Eurostat (Average number of rooms per person)
rooms_pers <- get_eurostat("ilc_lvho03", time_format = "num") %>%
  # Filter for Total tenure status and Total building type
  filter(tenure == "TOTAL", building == "TOTAL", !is.na(values)) %>%
  mutate(time = as.numeric(TIME_PERIOD),
         # Replaced oecdcountrycode to do custom match 
         # GRC and GBR were not available in prev version
         ref_area = countrycode::countrycode(geo, "iso2c", "iso3c", custom_match = c("EL" = "GRC", "UK" = "GBR"))) %>%   
  # Keep the latest available (non-missing) year for each country
  group_by(ref_area) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  # Keep only selected columns and data for OECD countries 
  select(ref_area, time_period = time, replace_indic = values) %>%
  drop_na(ref_area) %>%
  filter(ref_area %in% oecd_countries) %>%
  # Estimated values for AUS and ISR - ISR needs to be annually and AUS bi-annually updated
  # ISR CBS Social survey (2024 for 2025 BLI update), calculation of rooms per person from housing density
  add_row(
    ref_area = c("ISR"),
    time_period = c(2024),
    replace_indic = c(1.0965)
  ) %>%
  arrange(ref_area, time_period) %>%
  rbind(aus_vals)

#Adding continent or regions and their interactions do NOT significantly improve fit 
scatterplotCompare(hsl_dat, rooms_pers, "3_1")

overcrowding_imputed <- imputatorFunction(hsl_dat, rooms_pers, "3_1", imputing_iso3c, "baseline") 

#Imputed values
indic_3_1 <- rooms_pers %>%
  select(ref_area, time_period) %>%
  merge(overcrowding_imputed$indic_dat) %>%
  mutate(obs_status= "E") %>%
  rbind(hsl_dat %>% filter(measure == "3_1"))

rm(overcrowding_imputed, rooms_pers, aus_vals, imputing_iso3c, overcrowding_countries)

# 3_2 Housing affordability  ----------------------------------------------
# consistent: Unconditional mean

most_common_year_indic <- hsl_dat %>%
  filter(measure == "3_2") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

affordability_short <- hsl_dat %>% filter(measure == "3_2", ref_area %in% oecd_countries) 

countries_avail <- affordability_short %>% pull(ref_area)
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# CHE, ISL and ISR missing

# Replacing missing with average values (no better options found until now - (also tested with housing expenditure))
affordability_short <- affordability_short %>% summarise(obs_value = mean(obs_value)) %>% pull()

indic_3_2 <- tibble(
  ref_area = imputing_iso3c,
  obs_value = affordability_short,
  measure = "3_2",
  obs_status = "E",
  time_period = most_common_year_indic) %>% 
  rbind(hsl_dat %>% filter(measure == "3_2"))

rm(affordability_countries, affordability_short)

# 4_1 Time off ---------------------------------------------------------
# Average annual hours actually worked per worker (employees)
# https://data-explorer.oecd.org/vis?lc=en&tm=hours%20worked&pg=0&snb=59&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HW%40DF_AVG_ANN_HRS_WKD&df[ag]=OECD.ELS.SAE&df[vs]=1.0&isAvailabilityDisabled=false&dq=AUS%2BAUT%2BBEL%2BCAN%2BCHL%2BCOL%2BCRI%2BCZE%2BDNK%2BEST%2BFIN%2BFRA%2BDEU%2BGRC%2BHUN%2BISL%2BIRL%2BISR%2BITA%2BJPN%2BKOR%2BLVA%2BLTU%2BLUX%2BMEX%2BNLD%2BNZL%2BNOR%2BPOL%2BPRT%2BSVK%2BSVN%2BESP%2BSWE%2BCHE%2BTUR%2BGBR%2BUSA%2BOECD........ICSE93_1....&pd=2010%2C&to[TIME_PERIOD]=false&vw=tb
# Stronger correlation for employees (-0.74 without ITA outlier), rather than employed (-0.60) average hours actually worked
# Stronger than average weekly hours usually worked

# Time off refers to 2009 (n = 6), but when tested better latest year for predictor
most_common_year_indic <- hsl_dat %>%
  filter(measure == "4_1") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "4_1") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# 16 countries: "CHE" "CHL" "COL" "CRI" "CZE" "DNK" "ISL" "ISR" "LTU" "LUX" "LVA" "MEX" "PRT" "SVK" "SVN" "SWE"

# Latest year
weekly_hours_worked <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_HW@DF_AVG_ANN_HRS_WKD,1.0/........ICSE93_1....?&dimensionAtObservation=AllDimensions") %>%
  rsdmx::readSDMX() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  filter(ref_area %in% c(oecd_countries, partner_countries)) %>%
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(ref_area, replace_indic = obs_value) 

# ITA is a severe outlier - No additional regional nor continent needed
scatterplotCompare(hsl_dat, weekly_hours_worked, "4_1")

outliers_removed <- c("ITA", "FRA", "ESP", "AUS")
scatterplotCompare(hsl_dat, weekly_hours_worked %>% filter(!ref_area %in% outliers_removed), "4_1")
# Model best with AUT + ITA removed
scatterplotCompare(hsl_dat, weekly_hours_worked %>% filter(!ref_area %in% c("AUT", "ITA")), "4_1")

# Imputation process
time_off_imputed <- imputatorFunction(hsl_dat, 
                                      weekly_hours_worked %>% filter(!ref_area%in% outliers_removed), 
                                      "4_1", 
                                      imputing_iso3c) 

# Imputed values (used most_common_year_indic as strange to refer to the latest year)
indic_4_1 <- time_off_imputed$indic_dat %>%
  mutate(obs_status = "E", 
         time_period = most_common_year_indic) %>%
  rbind(hsl_dat %>% filter(measure == "4_1"))

rm(weekly_hours_worked, weekly_hours_worked_no_outlier, time_off_imputed,
   most_common_year_indic, countries_avail, imputing_iso3c)

# 4_3 Gender gap in hours worked ------------------------------------------
# New 2025: LTUR M with pre-imputation using UR M for missing value for CHL - using twice model with regional factors and interacting factors only in final model
# Correlation with latest LTUR M is 0.79 (with LTUR total is 0.76 and with LTUR F is 0.66 - correlation is higher for more recent years)
# Tested also with gender wage gap (corr= 0.03), UR (T, M, F - corr below 0.6) and with gender gap in ER and UR (below 0.2 and 0.5 respectively)

# Most common year = 2009 (same as for all indicators calculated from TUS)
most_common_year_indic <- hsl_dat %>%
  filter(measure == "4_3") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "4_3") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# "CHE" "CHL" "COL" "CRI" "CZE" "DNK" "ISL" "ISR" "LTU" "LVA" "MEX" "PRT" "SVK" "SVN"
# 14 countries (as other indicators based on TUS)

# STEP 1 -- IMPUTATION OF LTUR M for CHL (as only missing) using UR M

# Select latest year for LTUR M from full dataset
lturm <- full_dat%>%
  filter(sex == "M", age == "_T", education_lev == "_T", measure == "2_3",
         ref_area %in% c(oecd_countries, partner_countries)) %>%
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(ref_area, measure, obs_value, time_period)

# Retrieve UR M to impute LTUR M missing for only CHL
urm <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,/.UNE_RATE..M._T.UNE?&dimensionAtObservation=AllDimensions") %>%
  rsdmx::readSDMX() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  filter(ref_area %in% c(oecd_countries, partner_countries)) %>%
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(ref_area, replace_indic = obs_value, time_period)

# Model with regional factors (no interactions) is the best fit (even when compared to continent model)
scatterplotCompare(lturm, urm, "2_3")

#Imputation for LTUR M CHL using model with regional areas factors
lturm_imputed <- imputatorFunction(lturm, urm, "2_3", "CHL", model_type = "region_group")

augmented_2_3 <- lturm %>%
  select(-time_period) %>%
  rbind(lturm_imputed$indic_dat) %>%
  rename(replace_indic = obs_value) %>%
  select(-measure)

# STEP 2 - IMPUTING values for 4_3 using LTUR M (with imputation for CHL)

#Scattercompare with regional factors improve the fit
scatterplotCompare(hsl_dat, augmented_2_3, "4_3")

gap_hrswk_imputed <- imputatorFunction(hsl_dat, augmented_2_3, "4_3", imputing_iso3c, "region_inter")

# Combine imputed + original values
indic_4_3 <- gap_hrswk_imputed$indic_dat %>%
  mutate(obs_status = "E", 
         time_period = most_common_year_indic) %>%
  rbind(hsl_dat %>% filter(measure == "4_3"))

rm(gap_hrswk_imputed, augmented_2_3, lturm_imputed, urm, lturm, most_common_year_indic,
   imputing_countries, countries_avail)

# 7_2 Social interactions -----(New 2025: Average annual hrs actually worked (employees) 2014 with continent interacting factors)--------------------------------------------
# Note: Weak correlation (below 0.35) with social support (2024 and 2009 (most common year of 7_2)), satisfaction with personal relationships, loneliness, life satisfaction (from the HIL database), 
# Average hrs worked, % of individuals using computer, internet, internet daily, for socialnetworking sites, TV streams, playing
# PISA 2012 and 2015 on average minutes spent on internet by pupils
# WVS - Importance of family, friends, religion

most_common_year_indic <- hsl_dat %>%
  filter(measure == "7_2") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "7_2") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# 14 missing: "CHE" "CHL" "COL" "CRI" "CZE" "DNK" "ISL" "ISR" "LTU" "LVA" "MEX" "PRT" "SVK" "SVN"

# 2014 annual hours actually worked for complete coverage of missing countries (2009 or 2010 would not cover CRI)
weekly_worked_2014 <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.ELS.SAE,DSD_HW@DF_AVG_ANN_HRS_WKD,/........ICSE93_1....?&dimensionAtObservation=AllDimensions") %>%
  rsdmx::readSDMX() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>%
  filter(ref_area %in% c(oecd_countries, partner_countries)) %>%
  # instead of latest year try the most common year
  group_by(ref_area) %>%
  filter(time_period == "2014") %>%
  ungroup() %>%
  select(ref_area, replace_indic = obs_value)

# Scattercompare with interaction factor of continent grouping is best model
scatterplotCompare(hsl_dat, weekly_worked_2014, "7_2")

social_int_imputed <- imputatorFunction(hsl_dat, weekly_worked_2014, "7_2", imputing_iso3c, model_type = "cont_inter")

# Combine imputed + original values
# Time period as most common year (as 2014 serves only as backup year for country order)
indic_7_2 <- social_int_imputed$indic_dat %>%
  mutate(obs_status = "E", 
         time_period = most_common_year_indic) %>%
  rbind(hsl_dat %>% filter(measure == "7_2"))

rm(social_int_imputed, weekly_worked_2014, imputing_iso3c, countries_avail, most_common_year_indic)

# 8_1_DEP No say in government --------------------------------------------
# Notes: New in 2025: 2-step imputation with SDG 16.7.2 (External political efficacy - 
# Decision making is inclusive - European Social Survey, ESS) and 
# WVS7 - Politics is not at all important in life (TUR, USA) and adjustment factor
# 
# List of tests conducted (on the basis of OECD and academic literature) - 
# Reported here by decreasing correlation:
#1. SDG 16.7.2 - External political efficacy (corr= -0.94) - TUR and USA missing
#2. WVS 2017-2022: Importance in life: politics - not at all important (-0.58) - No data on External Political efficacy for TUR (base for SDG) - first time introduced
#3. WB Worldwide Governance Indicators (WGI) : highest correlation with Government Effectiveness  (-0.45) and its component bps- Business Enterprise Environment Survey (0.61) - this subcomponent has too few observations, lower with Voice and Accountability(-0.34)
#4. Gallup 2025 will of the people (corr: -0.46), TUR missing - the voice of people government index 2017 (corr<-0.26)
#5.trust in gov (corr: -0.406) -> Higher trust in parliament, political parties, and government → greater perception of influence.
#6. Perception of corruption (-0.302)
#7. CSES module 5 - internal political efficacy - neutral option highest corr (0.3)
#8. GaG 2023 indicators: low correlation or for those just below correl=0.60 same geographical coverage
#9. PIAAC previous political efficacy: low correlation (0.27)
#10. VTO (closest and most recent year: -0.169)-> Voter turnout, petition signing, demonstrations, contacting officials->Higher participation correlates with feeling empowered
#11. Stakeholder engagement (also Kate tried) correlation was 0.02

most_common_year_indic <- hsl_dat %>%
  filter(measure == "8_1_DEP") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period)

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "8_1_DEP") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# 6 countries: HUN, ISR, LTU, POL, TUR, USA

# SDG 16.7.2 Excel data.xlsx to download from https://unstats.un.org/sdgs/dataportal/database
#Just remember to choose "Countries or areas" instead of the default "All Groupings"for "Countries, areas or regions" - There should be (Selected 84 of 84)

# Step 1: Download "Goal16" from API
series_code_unsdg <- wiser::unsdg_code()

sdg16 <- wiser::unsdg_labeled_data(series_code_unsdg$SeriesCode[665]) %>%
  janitor::clean_names() %>%
  filter(
    series_code == "IU_DMK_INCL",
    age == "ALLAGE",
    education_level == "_T",
    location == "ALLAREA",
    population_group == "TOTAL",
    sex == "BOTHSEX",
    disability_status == "_T"
  ) %>%
  mutate(ref_area = countrycode::countrycode(geo_area_name, "country.name", "iso3c"),
         value = as.numeric(value),
         time_period = as.numeric(time_period)) %>%
  select(ref_area, time_period, value) %>%
  # Step 5: Create sdg16_reduced with latest Value for each ref_area
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(ref_area, replace_indic = value, time_period) 


# WVS7 Importance of politics in Life

wvs7 <- read_excel(paste0(mainpath, "/data/Important_in_life_Politics.xls"), sheet = "Hoja1") %>%
  slice(4,8) %>%
  janitor::row_to_names(1) %>%
  rename(measure = 1, total = 2) %>%
  pivot_longer(!c(measure), names_to = "ref_area", values_to = "obs_value",
               values_transform = list(obs_value = as.numeric)) %>%
  mutate(ref_area = countrycode::countrycode(ref_area, "country.name", "iso3c")) %>%
  drop_na() %>%
  select(ref_area, replace_indic = obs_value) %>%
  filter(ref_area %in% c(oecd_countries, partner_countries))

# STEP 1 - Decision making is inclusive
scatterplotCompare(hsl_dat, sdg16, "8_1_DEP")

# STEP 2 - Importance of politics in life - (TUR and USA)
scatterplotCompare(hsl_dat, wvs7, "8_1_DEP")

not_say_imputed1 <- imputatorFunction(hsl_dat, sdg16, "8_1_DEP", imputing_iso3c) 
not_say_imputed2 <- imputatorFunction(hsl_dat, wvs7 %>% filter(!ref_area == "MEX"), "8_1_DEP", imputing_iso3c) 

#Imputed values
no_say_imputations1 <- not_say_imputed1$indic_dat %>%
  mutate(obs_status= "E") %>%
  left_join(sdg16 %>% select(ref_area, time_period))

#As available from the World Values Survey wave 7: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp
no_say_imputations2 <- not_say_imputed2$indic_dat %>%
  mutate(obs_status= "E") %>%
  left_join(tibble(ref_area = c("TUR", "USA"), time_period = c(2018, 2017)))

which(unique(no_say_imputations1$ref_area) == unique(no_say_imputations2$ref_area))

#Rescaling values from the second step to align with the first step (best model fit)
impute_value_1 <- no_say_imputations1 %>% filter(ref_area == "POL") %>% pull(obs_value)
impute_value_2 <- no_say_imputations2 %>% filter(ref_area == "POL") %>% pull(obs_value)

# calculate the ratio for POL (only overlapping country)
ratio <- impute_value_1 / impute_value_2

# create d2 with adjusted values
# keep only ref_areas missing from d1
indic_8_1_dep <- no_say_imputations2 %>%
  mutate(obs_value = obs_value * ratio) %>%
  filter(!ref_area %in% no_say_imputations1$ref_area) %>%
  # bind rows: d1 unchanged + missing ref_areas from d2_scaled
  bind_rows(no_say_imputations1) %>%
  arrange(ref_area) %>%
  rbind(hsl_dat %>% filter(measure == "8_1_DEP"))


rm(ratio, impute_value_1, impute_value_2, no_say_imputations1, no_say_imputations2,
   not_say_imputed1, not_say_imputed2, wvs7, sdg16, series_code_unsdg, 
   most_common_year_indic, countries_avail, imputing_iso3c)

# 11_1 Life satisfaction --------------------------------------------------
# New 2025: Gallup life satisfaction 2-year average of the latest years
# Notes: Kate tried with Negative affect balance, but correlation very low (0.33)
# Download Gallup Excel for Life Today from Gallup Analytics in off years

# Pull countries to be imputed
countries_avail <- hsl_dat %>% filter(measure == "11_1") %>% distinct(ref_area) %>% pull 
imputing_iso3c <- oecd_countries[!oecd_countries %in% countries_avail]
# 4 countries: CHL, CRI, ISR, USA

#2024 is the most common year
most_common_year_indic11_1 <- hsl_dat %>%
  filter(measure == "11_1") %>%
  count(time_period, sort = T) %>%
  slice_head(n=1) %>%
  pull(time_period) %>%
  as.numeric

# Pull from analytics or microdata
life_sat_gallup <- readRDS(paste0(mainpath, "/data/11_1_gallup_life_sat.RDS")) %>%
  select(ref_area = country, time_period = year, replace_indic = obs_value) %>%
  filter(ref_area %in% c(oecd_countries, partner_countries)) 

avg_multi_year <- life_sat_gallup %>%
  mutate(
    bands = case_when(
      time_period %in% c(2024:2025) ~ 8,
      time_period %in% c(2022:2023) ~ 7,
      time_period %in% c(2020:2021) ~ 6,
      time_period %in% c(2017:2019) ~ 5,
      time_period %in% c(2014:2016) ~ 4,
      time_period %in% c(2011:2013) ~ 3,
      time_period %in% c(2008:2010) ~ 2,
      time_period %in% c(2006:2007) ~ 1
    )) %>%
  group_by(ref_area, bands) %>%
  mutate(replace_indic = mean(replace_indic)) %>%
  ungroup() %>%
  select(-bands) %>%
  group_by(ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup()

scatterplotCompare(hsl_dat, life_sat_gallup %>% filter(time_period == most_common_year_indic11_1) ,"11_1") 
scatterplotCompare(hsl_dat, avg_multi_year, "11_1")

life_satis_imputed <- imputatorFunction(hsl_dat, avg_multi_year, "11_1", imputing_iso3c, model_type = "cont_group")

#Imputed data only  & most common year chosen to be shown as time_period just as more practical than t-t-1
indic_11_1 <- life_satis_imputed$indic_dat %>% 
  mutate(obs_status= "E", time_period = most_common_year_indic11_1) %>%
  rbind(hsl_dat %>% filter(measure == "11_1"))

rm(countries_avail, imputing_iso3c, most_common_year_indic11_1, life_sat_gallup, 
   avg_multi_year, life_satis_imputed)

# -------------------------------------------------------------------------
# Final cleaning

#Grouping all indicators imputed with imputations
headline_imputations <- rbind(indic_1_1, indic_1_2, indic_1_3, indic_2_7, 
                              indic_3_1, indic_3_2, indic_4_1, indic_4_3, 
                              indic_7_2, indic_8_1_dep, indic_11_1)

# Final set of headline indicators for BLI calculation for OECD countries only
headline_final <-rbind (bli_dat, headline_imputations) %>% filter(ref_area %in% oecd_countries)  

# Save final version  
saveRDS(headline_final, paste0(mainpath, "/data/imputed_headline_dat.RDS"))

if(versioning == "yes") {
  saveRDS(headline_final, paste0(mainpath, "/versioning/imputed_headline_dat", Sys.Date() %>% format("%Y-%m-%d"),".RDS"))
}

