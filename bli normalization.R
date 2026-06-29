library(tidyverse)

mainpath <- "S:/Data/WDP/Well being database/Data Monitor/data_monitor/"

headline_final_oecdr <- readRDS("S:/Data/WDP/Well being database/Data Monitor/data_monitor/data/imputed_headline_dat.RDS")

# Define measures to rank descending (1 = best for higher values)
desc_measures <- c("1_1", "1_3", "2_1", "3_2", "4_1", "5_1", "6_2", "7_2", "8_2", "10_2_gap", "11_1")

# --- Check for missing values ---
if (any(is.na(headline_final_oecdr$obs_value))) {
  missing_cases <- headline_final_oecdr %>%
    filter(is.na(obs_value)) %>%
    select(ref_area, measure)
  stop("Missing obs_value found for: ", paste0(capture.output(print(missing_cases)), collapse = "\n"))
}


# --- Compute min, max, and z_value for each indicator ---
ind_norm <- headline_final_oecdr %>%
  mutate(obs_value = as.numeric(obs_value)) %>%
  group_by(measure) %>%
  mutate(
    min_value = min(obs_value, na.rm = TRUE),
    max_value = max(obs_value, na.rm = TRUE),
    z_value = if_else(
      measure %in% desc_measures,
      ((obs_value - min_value) / (max_value - min_value))*10,
      (1 - ((obs_value - min_value) / (max_value - min_value)))*10
    )
  ) %>%
  ungroup()

# ---  Extract dimension number ---
ind_norm <- ind_norm %>%
  mutate(dim = str_extract(measure, "^[0-9]+"))

# --- Compute dim_value (average z_value per ref_area and dim) ---
dim_avg <- ind_norm %>%
  group_by(ref_area, dim) %>%
  summarise(dim_value = mean(z_value, na.rm = TRUE), .groups = "drop")

# Merge dim_value back into ind_norm
ind_norm <- ind_norm %>%
  left_join(dim_avg, by = c("ref_area", "dim"))

# --- Compute BLI_eq_value: final aggregated BLI with equal weights ---
BLI_eq_value <- dim_avg %>%
  group_by(ref_area) %>%
  summarise(BLI_eq = mean(dim_value, na.rm = TRUE), .groups = "drop")

#BLI sorted with best on top
BLI_sorted <- BLI_eq_value %>% 
  arrange(rank_BLI)

#SAVE these (RDS and Excel) as they allow quick checks and follow ups
saveRDS(ind_norm, paste0(mainpath, "Output/BLI_ind_norm.RDS"))
saveRDS(BLI_sorted, paste0(mainpath, "Output/BLI_sorted.RDS"))

# library(writexl)
# write_xlsx(ind_norm, paste0(mainpath,"Output/BLI_ind_norm.xlsx"))
# write_xlsx(BLI_sorted, paste0(mainpath,"Output/BLI_sorted.xlsx"))



library(dplyr)

# --- Identify measures where min_value or max_value correspond to obs_status = "E" ---
# REMEMBER TO CHECK IF THESE WERE PREVIOUSLY FLAGGED AS E IN DATA EXPLORER (= NOT IMPUTATIONS)
E_flagged_extremes <- ind_norm %>%
  group_by(measure) %>%
  summarise(
    min_E_ref = paste(ref_area[obs_value == min_value & obs_status == "E"], collapse = ", "),
    max_E_ref = paste(ref_area[obs_value == max_value & obs_status == "E"], collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(min_E_ref != "" | max_E_ref != "")

# --- Print formatted summary ---
if (nrow(E_flagged_extremes) > 0) {
  message("Measures where min_value or max_value are associated with obs_status = 'E':")
  
  apply(E_flagged_extremes, 1, function(x) {
    measure <- x["measure"]
    min_refs <- x["min_E_ref"]
    max_refs <- x["max_E_ref"]
    
    if (min_refs != "" & max_refs != "") {
      cat(sprintf("Measure %s | min_value and max_value flagged (E) | ref_areas: min→%s | max→%s\n",
                  measure, min_refs, max_refs))
    } else if (min_refs != "") {
      cat(sprintf("Measure %s | min_value flagged (E) | ref_areas: %s\n", measure, min_refs))
    } else if (max_refs != "") {
      cat(sprintf("Measure %s | max_value flagged (E) | ref_areas: %s\n", measure, max_refs))
    }
  })
  
} else {
  message("No measures found where min_value or max_value are associated with obs_status = 'E'.")
}

E_flagged_extremes
#Saved Excel for reference
write_xlsx(E_flagged_extremes, paste0(mainpath,"Checks/E_flagged_extremes.xlsx"))

#ORGANISE AND EXPORT DATA for Data Monitor
library(dplyr)
library(tidyr)
library(oecdcountrycode)
library(readr)
library(writexl)

# Create mapping between dim numbers and their corresponding names
dim_labels_web <- c(
  "1"  = "income",
  "2"  = "jobs",
  "3"  = "housing",
  "4"  = "balance",
  "5"  = "health",
  "6"  = "education",
  "7"  = "community",
  "8"  = "civic",
  "9"  = "environment",
  "10" = "safety",
  "11" = "satisfaction"
)

# Define desired column order
col_order <- c(
  "country", "housing", "income", "jobs", "community",
  "education", "environment", "civic", "health",
  "satisfaction", "safety", "balance"
)

# Transform dataset
data <- dim_avg %>%
  mutate(
    # Convert dim numeric code to corresponding label
    dim = dim_labels_web[as.character(dim)],
    # Convert ISO3 code to official country name
    country = oecdcountrycode(ref_area, "iso3c", "country.name")
  ) %>%
  select(country, dim, dim_value) %>%
  # Pivot to wide format
  pivot_wider(
    names_from = dim,
    values_from = dim_value
  ) %>%
  # Arrange alphabetically by country
  arrange(country) %>%
  # Reorder columns as specified
  select(any_of(col_order))

#Export Excel as a backup
write_xlsx(data, paste0(mainpath, "Output/data.xlsx"))

# Export to TSV format
write_tsv(data, paste0(mainpath, "Output/data.tsv"))
