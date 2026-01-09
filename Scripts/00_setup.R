############################################################
# 00_setup.R
# Purpose:
#  - Load WVS7 data (CSV)
#  - Keep required variables
#  - Recode missings (negative codes, -9999, NaN/Inf)
#  - Create analysis variables (female, region_id)
#  - Create TWO cleaned datasets:
#       (1) base: for variance decomposition + age/gender models
#       (2) income: for income slope + gini interaction models
#  - Run nestedness sanity check: regions per country
#  - Save cleaned data as RDS
############################################################

library(readr)
library(dplyr)
library(knitr)

# ----------------------------------------------------------
# 1) Load data
# ----------------------------------------------------------
wvs <- read_csv("Data/WVS_Cross-National_Wave_7_csv_v6_0.csv", show_col_types = FALSE)

# ----------------------------------------------------------
# 2) Select + rename variables (keep only what we need)
# ----------------------------------------------------------
wvs_sel <- wvs %>%
  select(
    life_satisfaction = Q49,              # 1-10
    age               = Q262,             # age in years
    sex               = Q260,             # 1=male, 2=female
    income            = Q288,             # income deciles 1-10
    country_code      = B_COUNTRY,        # numeric country code
    country_iso       = B_COUNTRY_ALPHA,  # country code (ISO)
    region_iso        = N_REGION_ISO,     # numeric region code; negatives = missing
    gini              = giniWB            # country-level Gini (often -9999 for missing)
  )

# ----------------------------------------------------------
# 3) Helper: convert special codes to NA (incl. NaN/Inf)
# ----------------------------------------------------------
to_na_wvs <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  # Defensive: convert NaN/Inf to NA
  x[is.nan(x) | is.infinite(x)] <- NA
  
  # WVS missing codes often negative; giniWB often uses -9999
  x[x < 0] <- NA
  
  x
}

# ----------------------------------------------------------
# 4) Recode variables + create derived variables
# ----------------------------------------------------------
wvs_recoded <- wvs_sel %>%
  mutate(
    life_satisfaction = to_na_wvs(life_satisfaction),
    age               = to_na_wvs(age),
    sex               = to_na_wvs(sex),
    income            = to_na_wvs(income),
    region_iso        = to_na_wvs(region_iso),
    gini              = to_na_wvs(gini),
    
    # Grouping variables
    country_code = as.factor(country_code),
    country_iso  = as.factor(country_iso),
    
    # Female dummy (0/1)
    female = case_when(
      sex == 2 ~ 1,
      sex == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Ensure regions are nested in countries (unique id)
    region_id = interaction(country_iso, region_iso, drop = TRUE)
  )

# -----------------------------------------------------------------
# 5A) BASE dataset (for null model + age/gender models)
#     Keep the sample as broad as possible: do NOT require income/gini
# -----------------------------------------------------------------
wvs_clean_base <- wvs_recoded %>%
  filter(
    between(life_satisfaction, 1, 10),
    age >= 16 & age <= 100,
    !is.na(female),
    !is.na(country_code),
    !is.na(country_iso),
    !is.na(region_id)
  ) %>%
  mutate(
    # Grand-mean centering within BASE sample
    age_c  = age - mean(age, na.rm = TRUE),
    age_c2 = age_c^2
  )

# -----------------------------------------------------------------
# 5B) INCOME dataset (for income slope + gini interaction models)
#     Restrict to complete cases for income and gini
# -----------------------------------------------------------------
wvs_clean_income <- wvs_clean_base %>%
  filter(
    between(income, 1, 10),
    !is.na(gini)
  ) %>%
  mutate(
    # Grand-mean centering within INCOME sample
    age_c    = age - mean(age, na.rm = TRUE),
    age_c2   = age_c^2,
    income_c = income - mean(income, na.rm = TRUE),
    gini_c   = gini - mean(gini, na.rm = TRUE)
  )

# ----------------------------------------------------------
# 6) Sanity check: regions per country (nested structure)
# ----------------------------------------------------------
regions_per_country <- wvs_clean_base %>%
  distinct(country_code, region_id) %>%     # one row per (country, region)
  count(country_code, name = "n_regions")

cat("=== Nestedness check (BASE sample): regions per country ===\n")
print(summary(regions_per_country$n_regions))
cat("Countries with exactly 1 region:", sum(regions_per_country$n_regions == 1), "\n\n")

top10_regions <- regions_per_country %>%
  arrange(desc(n_regions)) %>%
  slice_head(n = 10)

cat("Top 10 countries by number of regions (BASE):\n")
print(top10_regions)
cat("\n")

# ----------------------------------------------------------
# 7) Quick sanity checks (sample sizes)
# ----------------------------------------------------------
cat("=== BASE sample ===\n")
cat("N rows:", nrow(wvs_clean_base), "\n")
cat("N countries:", n_distinct(wvs_clean_base$country_code), "\n")
cat("N regions:", n_distinct(wvs_clean_base$region_id), "\n\n")

cat("=== INCOME sample ===\n")
cat("N rows:", nrow(wvs_clean_income), "\n")
cat("N countries:", n_distinct(wvs_clean_income$country_code), "\n")
cat("N regions:", n_distinct(wvs_clean_income$region_id), "\n\n")

# ----------------------------------------------------------
# Diagnostic check: compare original vs base vs income samples
# ----------------------------------------------------------
# Define a comparable "original" sample (subselected variables, minimal cleaning)
# Note: we do NOT require region_id here; just keep basic fields for comparability
wvs_original_comp <- wvs_sel %>%
  mutate(
    life_satisfaction = to_na_wvs(life_satisfaction),
    age = to_na_wvs(age),
    sex = to_na_wvs(sex),
    female = case_when(
      sex == 2 ~ 1,
      sex == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(
    between(life_satisfaction, 1, 10),
    age >= 16 & age <= 100,
    !is.na(female)
  ) %>%
  mutate(sample = "Original")

diagnostic_table <- bind_rows(
  wvs_original_comp %>% select(sample, life_satisfaction, age, female),
  wvs_clean_base   %>% mutate(sample = "Base")   %>% select(sample, life_satisfaction, age, female),
  wvs_clean_income %>% mutate(sample = "Income") %>% select(sample, life_satisfaction, age, female)
) %>%
  group_by(sample) %>%
  summarise(
    n = n(),
    mean_life_satisfaction = mean(life_satisfaction, na.rm = TRUE),
    sd_life_satisfaction   = sd(life_satisfaction, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    pct_female = 100 * mean(female, na.rm = TRUE),
    .groups = "drop"
  )

diagnostic_table
# ----------------------------------------------------------
# 8) Save processed data
# ----------------------------------------------------------
saveRDS(wvs_clean_base,   "Data/wvs_clean_base.rds")
saveRDS(wvs_clean_income, "Data/wvs_clean_income.rds")
saveRDS(regions_per_country, "Data/regions_per_country_base.rds")
saveRDS(diagnostic_table, "Data/diagnostic_table.rds")

message("Setup completed. Outputs saved in Data/.")

