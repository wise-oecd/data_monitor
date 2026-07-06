library(tidyverse)
library(pdftools)

# ── 1. Load & parse PDF ────────────────────────────────────────────────────────
# dat <- pdf_text("https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf")
dat <- pdf_text("S:/Data/WDP/Well being database/Metadata/oecd-well-being-database-definitions_june 2026.pdf")

full_text <- dat %>%
  paste(collapse = "\n") %>%
  str_split("\n") %>% .[[1]] %>%
  trimws() %>%
  paste(collapse = "\n")

# ── 2. Split into per-indicator blocks ────────────────────────────────────────
blocks <- str_split(full_text, "(?=\n\\d+\\.\\d+ [A-Z])")[[1]]
blocks  <- blocks[str_detect(blocks, "Indicator and unit of measurement:")]

# ── 3. Parse each block ───────────────────────────────────────────────────────
parse_block <- function(block) {
  
  first_line <- str_split(block, "\n")[[1]] %>% trimws() %>% .[nchar(.) > 0] %>% .[1]
  code       <- str_extract(first_line, "^\\d+\\.\\d+")
  measure    <- str_replace_all(code, "\\.", "_")
  label      <- str_remove(first_line, "^\\d+\\.\\d+\\s+") %>% trimws()
  
  # Full "Indicator and unit of measurement:" field
  ind_raw <- block %>%
    str_extract("(?<=Indicator and unit of measurement:)[\\s\\S]*?(?=\nType of indicator:)") %>%
    str_replace_all("\\s+", " ") %>% trimws()
  
  # measures where the comma should NOT split indicator vs unit
  no_split <- c("1_4", "2_4", "2_5", "2_9", "3_2", "5_3", 
                "11_1", "4_2", "4_3", "4_4", "7_3")   
  
  split_comma <- str_detect(ind_raw, ",") & !(measure %in% no_split)
  
  indicator <- if_else(split_comma,
                       str_extract(ind_raw, "^[^,]+") %>% trimws(),
                       ind_raw)
  unit      <- if_else(split_comma,
                       str_remove(ind_raw, "^[^,]+,") %>% trimws(),
                       ind_raw)
  
  # Full definition block (between Definition: and Source:)
  definition <- block %>%
    str_extract("(?<=\nDefinition:)[\\s\\S]*?(?=\nSource:)") %>%
    str_replace_all("\\s+", " ") %>% trimws()
  
  abbr_guard <- "(?<!\\bi\\.e)(?<!\\be\\.g)(?<!\\betc)(?<!\\bi\\.e\\.)(?<!\\be\\.g\\.)"
  sent_end   <- "\\.(?=\\s+[A-Z]|\\s*$)"
  
  dep_sent  <- str_extract(definition, paste0("(?i)Deprivation.*?", abbr_guard, sent_end))
  vert_sent <- str_extract(definition, paste0("(?i)Vertical inequality.*?", abbr_guard, sent_end))
  
  tibble(
    measure    = measure,
    label      = label,
    indicator  = indicator,
    unit       = unit,
    definition = definition,
    dep_sent   = dep_sent,
    vert_sent  = vert_sent
  )
}

base <- blocks %>% map_dfr(parse_block) %>% filter(!is.na(measure))

# ── 4. Build DEP rows ─────────────────────────────────────────────────────────
# label/indicator/unit for DEP rows follow template patterns observed in the xlsx
dep_rows <- base %>%
  filter(!is.na(dep_sent)) %>%
  mutate(
    measure   = paste0(measure, "_DEP"),
    # label: template uses "Dis-/No-/Low-/Not-" prefix patterns — encode known ones
    label = case_when(
      str_detect(label, "(?i)life satisfaction")          ~ "Dissatisfaction with life",
      str_detect(label, "(?i)job satisfaction")           ~ "Dissatisfaction with job",
      str_detect(label, "(?i)time use")                   ~ "Dissatisfaction with time use",
      str_detect(label, "(?i)personal relationship")      ~ "Dissatisfaction with personal relationships",
      str_detect(label, "(?i)perceived health")           ~ "Poor perceived health",
      str_detect(label, "(?i)social support")             ~ "No social support",
      str_detect(label, "(?i)feeling safe")               ~ "Feeling unsafe at night",
      str_detect(label, "(?i)having a say")               ~ "Not having a say in government",
      str_detect(label, "(?i)trust in government")        ~ "No trust in the government",
      str_detect(label, "(?i)volunteering")               ~ "No volunteering",
      str_detect(label, "(?i)wages|earnings")             ~ "Low wages",
      str_detect(label, "(?i)student.*read|read.*student")~ "Students with low skills",
      str_detect(label, "(?i)student.*math|math.*student")~ "Students with low skills",
      str_detect(label, "(?i)student.*sci|sci.*student")  ~ "Students with low skills",
      str_detect(label, "(?i)adult.*numer|numer.*adult")  ~ "Adults with low skills",
      str_detect(label, "(?i)adult.*liter|liter.*adult")  ~ "Adults with low skills",
      TRUE ~ paste("Deprivation:", label)
    ),
    # indicator for DEP: use the extracted dep sentence as indicator, else fallback
    indicator = str_remove(dep_sent, "(?i)^Deprivation[^:]*:\\s*") %>%
      str_remove("\\.$") %>% trimws(),
    unit      = indicator   # template repeats indicator as unit for DEP rows
  )

# ── 5. Build VERT rows ────────────────────────────────────────────────────────
vert_rows <- base %>%
  filter(!is.na(vert_sent)) %>%
  mutate(
    measure   = paste0(measure, "_VER"),
    label = case_when(
      str_detect(label, "(?i)life satisfaction")          ~ "Life satisfaction inequality",
      str_detect(label, "(?i)job satisfaction")           ~ "Job satisfaction inequality",
      str_detect(label, "(?i)time use")                   ~ "Time use inequality",
      str_detect(label, "(?i)personal relationship")      ~ "Personal relationships inequality",
      str_detect(label, "(?i)household.*wealth|wealth")   ~ "Household net wealth inequality",
      str_detect(label, "(?i)wages|earnings")             ~ "Wage inequality",
      str_detect(label, "(?i)student.*read|read.*student")~ "Student reading skills inequality",
      str_detect(label, "(?i)student.*math|math.*student")~ "Student maths skills inequality",
      str_detect(label, "(?i)student.*sci|sci.*student")  ~ "Student science skills inequality",
      str_detect(label, "(?i)adult.*numer|numer.*adult")  ~ "Adult skills (numeracy)",
      str_detect(label, "(?i)adult.*liter|liter.*adult")  ~ "Adult skills (literacy)",
      TRUE ~ paste(label, "inequality")
    ),
    indicator = paste("Vertical inequality in", indicator),
    unit      = str_remove(vert_sent, "(?i)^Vertical inequality[^:]*:\\s*") %>%
      str_remove("\\.$") %>% trimws()
  )

# ── 6. Combine, drop helper cols, arrange ─────────────────────────────────────
result <- bind_rows(base, dep_rows, vert_rows) %>%
  select(measure, label, indicator, unit, definition) %>%
  mutate(definition = str_remove_all(definition, "WELL-BEING DATABASE: DEFINITIONS AND METADATA"),
         definition = str_remove_all(definition, "OECD HOW’S LIFE\\?"),
         definition = str_remove_all(definition, "\\ \\d+"),
         definition = str_remove_all(definition, "\\d+ \\"),
         definition = trimws(definition),
         definition = str_replace_all(definition, "•", "<br>•"),
         definition = str_replace_all(definition, "Due to the small", "<br><br>Due to the small"),
         definition = str_replace_all(definition, "Deprivation", "<br><br>Deprivation"),
         definition = str_replace_all(definition, "Vertical inequality", "<br><br>Vertical inequality"),
         unit = case_when(
           str_starts(unit, "as a ") ~ str_remove(unit, "^as a "),
           TRUE ~ unit
         ),
         unit = str_remove_all(unit, "Measured in"),
         unit = str_replace(unit, "^(.)", toupper),
         note = case_when(
           measure %in% c("1_3", "1_3_VER", "1_6", "8_2") ~ "The OECD average is calculated using a last observation carried forward approach, whereby each country’s most recent available observation is carried forward to 
                                               subsequent years until updated data become available. This method allows for the construction of a consistent time series for the OECD average in the presence of 
                                               gaps in country-level data and should be interpreted with this in mind. Information on which years include carried-forward values is available in the OECD Data Explorer, 
                                               accessible via the <b>OECD How's Life? database</b> link above.",
           measure %in% c("13_5") ~ "The OECD average is calculated using a last observation carried forward approach, whereby each country’s most recent available observation is carried forward to subsequent years until 
                                    updated data become available. This method allows for the construction of a consistent time series for the OECD average in the presence of gaps in country-level data and should be interpreted 
                                    with this in mind. Information on which years include carried-forward values is available in the OECD Data Explorer, accessible via the <b>OECD How's Life? database</b> link above.<br><br>The OECD 
                                    average is based on both self-reported and measured obesity rates. As outcomes differ by data collection methodology, the average should be interpreted with caution. Information on the methodology 
                                    used is available in the OECD How’s Life? Well-being Database metadata found <a href='https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf'>here.</a>")) %>%
  arrange(measure) 


openxlsx::write.xlsx(result, "\\\\FS19-AZ-CH-1.main.oecd.org/SdataWIS/Data/WDP/Well being database/Data Monitor/data_monitor/hows_life_dictionary.xlsx")

