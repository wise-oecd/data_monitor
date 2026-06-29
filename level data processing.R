library(tidyverse)
source("./global_processing.R")

# Pull break treated data and clean for analysis
tidy_dat <- readRDS("S:/Data/WDP/Well being database/Automated database/output/break_treated_final_dataset.RDS") %>% 
  # rbind(oecd_avg_dat) %>%
  filter(dimension == "_T", !measure %in% dropped_indics) %>%
  select(ref_area, time_period, measure, dimension, unit_measure, obs_value) %>%
  # mutate(unit_measure = ifelse(measure == "3_3", "PT_POP", unit_measure)) %>%
  left_join(dict %>% select(measure, threshold, direction), by=c("measure")) %>%
  distinct() 

# Define complete dataset to create "no data" cards and set order of indicators
# according to dimension
gap_filler <- tidy_dat %>%
  distinct(measure, unit_measure) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into=c("cat", "subcat")) %>%
  mutate(cat = as.numeric(cat),
         subcat = as.numeric(subcat)) %>%
  arrange(cat, subcat, measure) %>%
  select(-cat, -subcat) %>%
  mutate(measure = fct_infreq(measure)) %>%
  merge(., data.frame(ref_area = c(all_countries, "OECD")))

# Card data point calculations --------------------------------------------

# Calculate latest and earliest values for OECD average 

# Remove TUS values for OECD average
avg_oecd_dat <- tidy_dat %>% filter(time_period >= 2015, !measure %in% c("4_1", "4_2", "4_3", "7_2"))

avg_vals <- twoPointAverage(avg_oecd_dat, "_T") %>% 
  filter(!measure %in% oecd_avg_measures) %>%
  select(-label) %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) 

# Calculate average using TUS methodology
avg_vals <- twoPointAverageTUS(tidy_dat, "_T") %>%
  rbind(avg_vals)

# Set aside TUS data
tus_dat <- tidy_dat %>% filter(measure %in% c("4_1", "4_2", "4_3", "7_2"))
tidy_dat <- tidy_dat %>% filter(time_period >= 2015, !measure %in% c("4_1", "4_2", "4_3", "7_2")) %>% rbind(tus_dat)

# Combine with full dataset and define which values are earliest and latest
avg_vals <- tidy_dat %>% 
  rbind(avg_vals) %>%
  group_by(ref_area, measure) %>%
  filter(time_period == max(time_period) | time_period == min(time_period)) %>%
  mutate(label = case_when(
    time_period == max(time_period) ~ "latest", 
    TRUE ~ "earliest")
    ) %>%
  ungroup() 

# Calculate tier in latest period - only OECD countries are compared
tiers_dat <- tidy_dat %>%
  filter(ref_area %in% oecd_countries) %>%
  select(ref_area, time_period, obs_value, measure) %>%
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  left_join(dict %>% select(measure, direction)) %>%
  group_by(measure) %>%
  arrange(measure, obs_value) %>%
  # Exposure to extreme temps has many 0s which unfairly skew the ranking
  # Countries with 0 are not automatically tier 1
  # Use min to ties.method, fixes for 12.4 and some other values as well
  mutate(rank =
           case_when(
             direction == "positive" ~ rank(obs_value, ties.method = "min"),
             TRUE ~ rank(-obs_value, ties.method = "min")
           ),
         rank_max = max(rank)
  ) %>%
  ungroup() %>% 
  mutate(share = rank/rank_max,
         tiers = case_when(
           share < 0.33 ~ 3,
           share > 0.66 ~ 1,
           TRUE ~ 2
         )
  ) %>%
  ungroup() %>% 
  select(measure, ref_area, tiers) %>%
  distinct() %>%
  complete(measure = unique(tidy_dat$measure)) %>%
  group_by(measure) %>%
  complete(ref_area = all_countries) %>%
  ungroup() %>%
  mutate(icon = case_when(
    tiers == 1 ~ "1-circle-fill.png",
    tiers == 2 ~ "2-circle-fill.png",
    tiers == 3 ~ "3-circle-fill.png",
    TRUE ~ "three-dots.png"
  )) %>%
  drop_na(icon)

# Calculate point change and evaluate it wrt to the threshold
point_change_dat <- avg_vals %>%
  select(-time_period) %>%
  pivot_wider(names_from = "label", values_from = "obs_value") %>%
  mutate(point_change = latest - earliest,
         perf_val = case_when(
           direction == "positive" & point_change > threshold ~ "good",
           direction == "negative" & point_change < threshold * -1 ~ "good",
           direction == "positive" & point_change < threshold * -1 ~ "bad",
           direction == "negative" & point_change > threshold ~ "bad",
           direction == "positive" & (!point_change > threshold | !point_change < threshold * -1) ~ "neutral",
           direction == "negative" & (!point_change < threshold * -1 | !point_change > threshold ) ~ "neutral"
         ),
         perf_val = case_when(
           perf_val == "good" ~ "#0F8554",
           perf_val == "neutral" ~ "goldenrod",
           perf_val == "bad" ~ "#CF597E",
           TRUE ~ "#999999"
         )
         ) %>%
  select(-latest) %>%
  merge(gap_filler, by = c("ref_area", "measure", "unit_measure"), all = T) %>%
  merge(skills_sig, by = c("ref_area", "measure"), all = T) %>% 
  mutate(
    perf_val = case_when(
      sig == "0" & !is.na(perf_val) ~ "goldenrod",
      TRUE ~ perf_val)
    ) %>%
  select(-sig) %>%
  mutate(
    perf_val = case_when(
      is.na(perf_val) ~ "#999999",
      TRUE ~ perf_val
    ),
    perf_val_name = case_when(
      perf_val == "#0F8554" ~ "Improving",
      perf_val == "goldenrod" ~ "No significant change",
      perf_val == "#CF597E" ~ "Deteriorating",
      perf_val == "#999999" ~ "Not enough data",
    )
  )

# Pull out latest year and values
latest_dat <- avg_vals %>%
  filter(label == "latest") %>%
  select(measure, ref_area, latest_year = time_period, latest = obs_value) 

avg_vals %>% filter(measure == "4_1")

# Final cleaning 
avg_vals <- point_change_dat %>%
  # Create cat group for easier defining of well-being dimensions
  mutate(measure2 = measure) %>%
  separate(measure2, into="cat") %>%
  distinct() %>% 
  left_join(tiers_dat %>% select(-tiers), by = c("measure", "ref_area")) %>%
  left_join(latest_dat, by = c("measure", "ref_area")) %>% 
  merge(dict %>% select(measure, label_name = label, label_name_fr = label_fr, unit_tag,  unit_tag_fr, round_val, position), by = "measure") %>%
  ungroup() %>%
  mutate(
    # Set dimension icons for card
    image = case_when(
      cat == "1" ~ "income and wealth.png",
      cat == "3" ~ "housing.png",
      cat == "2" ~ "work and job quality.png",
      cat == "5" ~ "health.png",
      cat == "6" ~ "knowledge and skills.png",
      cat == "9" ~ "environmental quality.png",
      cat == "11" ~ "subjective wellbeing.png",
      cat == "10" ~ "safety.png",
      cat == "4" ~ "worklife balance.png",
      cat == "7" ~ "social connections.png",
      cat == "8" ~ "civic engagement.png",
      cat == "12" ~ "natural capital.png",
      cat == "13" ~ "human capital.png",
      cat == "14" ~ "social capital.png",
      cat == "15" ~ "economic capital.png"
    ),
    image_caption = str_remove_all(image, "\\.png"),
    image_caption = tools::toTitleCase(image_caption),
    image_caption_fr = case_when(
      cat == "1" ~ "Revenu et patrimoine",
      cat == "2" ~ "Travail et qualité de l'emploi",
      cat == "3" ~ "Logement",
      cat == "4" ~ "Équilibre travail-vie privée",
      cat == "5" ~ "Santé",
      cat == "6" ~ "Connaissances et compétences",
      cat == "7" ~ "Liens sociaux",
      cat == "8" ~ "Sécurité",
      cat == "9" ~ "Qualité environnementale",
      cat == "10" ~ "Engagement civique",
      cat == "11" ~ "Bien-être subjectif",
      cat == "12" ~ "Capital naturel",
      cat == "13" ~ "Capital humain",
      cat == "14" ~ "Capital social",
      cat == "15" ~ "Capital économique"
    ),
    # Set performance arrangement
    arrangement = case_when(
      icon == "1-circle-fill.png" & perf_val == "#0F8554" ~ 1,
      icon == "1-circle-fill.png" & perf_val == "goldenrod" ~ 2,
      icon == "1-circle-fill.png" & perf_val == "#CF597E" ~ 3,
      icon == "1-circle-fill.png" & perf_val == "#999999" ~ 4,
      icon == "2-circle-fill.png" & perf_val == "#0F8554" ~ 5,
      icon == "2-circle-fill.png" & perf_val == "goldenrod" ~ 6,
      icon == "2-circle-fill.png" & perf_val == "#CF597E" ~ 7,
      icon == "2-circle-fill.png" & perf_val == "#999999" ~ 8,
      icon == "3-circle-fill.png" & perf_val == "#0F8554" ~ 9,
      icon == "3-circle-fill.png" & perf_val == "goldenrod" ~ 10,
      icon == "3-circle-fill.png" & perf_val == "#CF597E" ~ 11,
      icon == "3-circle-fill.png" & perf_val == "#999999" ~ 12,
      icon == "three-dots.png" & perf_val == "#0F8554" ~ 13,
      icon == "three-dots.png" & perf_val == "goldenrod" ~ 14,
      icon == "three-dots.png" & perf_val == "#CF597E" ~ 15,
      icon == "three-dots.png" & perf_val == "#999999" & !is.na(latest) ~ 16,
      TRUE ~ 17
    ),
    # Set OECD arrangement
    arrangement = case_when(
      grepl("OECD", ref_area) & perf_val == "#0F8554" ~ 1,
      grepl("OECD", ref_area) & perf_val == "goldenrod" ~ 2,
      grepl("OECD", ref_area) & perf_val == "#CF597E" ~ 3,
      grepl("OECD", ref_area) & perf_val == "#999999" ~ 4,
      TRUE ~ arrangement
    ),
    # Set opacity of card when no data available
    opacity = case_when(
      is.na(latest) ~ 0.6,
      TRUE ~ 1
    ),
    latest_year = case_when(
      is.na(latest_year) ~ " ",
      TRUE ~ as.character(latest_year)
    )
  )

# Versioning
saveRDS(avg_vals, paste0("S:/Data/WDP/Well being database/Data Monitor/data_monitor/versioning/cwb_latest point data_", todays_date,".RDS"))

# For app use
saveRDS(avg_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page/data/latest point data.RDS")
saveRDS(avg_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page/data/latest point data.RDS")
saveRDS(avg_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page_fr/data/latest point data.RDS")
saveRDS(avg_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page_fr/data/latest point data.RDS")


gap_filler <- avg_vals %>%
  distinct(measure) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into=c("cat", "subcat")) %>%
  mutate(cat = as.numeric(cat),
         subcat = as.numeric(subcat)) %>%
  arrange(cat, subcat, measure) %>%
  select(-subcat) %>%
  mutate(measure = fct_infreq(measure))

saveRDS(gap_filler, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page/data/gap filler.RDS")
saveRDS(gap_filler, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page/data/gap filler.RDS")
saveRDS(gap_filler, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page_fr/data/gap filler.RDS")
saveRDS(gap_filler, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page_fr/data/gap filler.RDS")


# Time series calculation -------------------------------------------------

ts_vals <- timeSeriesAverage(tidy_dat, "_T") %>%
  filter(!measure %in% oecd_avg_measures) %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) %>%
  rbind(tidy_dat)

ts_vals <- ts_vals %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into="cat") %>%
  distinct() %>%
  merge(point_change_dat %>% select(ref_area, measure, perf_val), by = c("ref_area", "measure")) %>%
  left_join(dict %>% select(measure, round_val, unit_tag_clean, position), by = "measure") %>%
  mutate(
    obs_value_tidy = prettyNum(round(obs_value, round_val), big.mark = " "),
    obs_value_tidy = case_when(
      position == "after" & unit_tag_clean == "%" ~ paste0(obs_value_tidy, "%"),
      position == "before" ~ paste0("USD ", obs_value_tidy),
      position == "after" & !unit_tag_clean == "%" ~ paste0(obs_value_tidy, " ", unit_tag_clean),
    )
  ) %>%
  select(-unit_measure) 


# Versioning
saveRDS(ts_vals, paste0("S:/Data/WDP/Well being database/Data Monitor/data_monitor/versioning/cwb_time series_", todays_date,".RDS"))

# For app use
saveRDS(ts_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page/data/time series.RDS")
saveRDS(ts_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page/data/time series.RDS")
saveRDS(ts_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/cwb_page_fr/data/time series.RDS")
saveRDS(ts_vals, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/fwb_page_fr/data/time series.RDS")


