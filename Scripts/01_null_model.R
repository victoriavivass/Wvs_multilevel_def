############################################################
# 01_empty_model.R
# Purpose: Decomposition of variance in life satisfaction
#          using a three-level empty multilevel model
#          (individuals nested in regions nested in countries)
############################################################

library(lme4)
library(tibble)
library(dplyr)

# ----------------------------------------------------------
# 1) Load BASE dataset (no income / gini restrictions)
# ----------------------------------------------------------
wvs_clean_base <- readRDS("Data/wvs_clean_base.rds")

# ----------------------------------------------------------
# 2) Three-level empty model
# ----------------------------------------------------------
# Model:
#   y_irc = β0 + u0c + u0rc + e_irc
#

m0_3level <- lmer(
  life_satisfaction ~ 1 +
    (1 | country_code) +
    (1 | region_id),
  data = wvs_clean_base,
  REML = TRUE
)

summary(m0_3level)

# ----------------------------------------------------------
# 3) Extract variance components
# ----------------------------------------------------------
vc <- VarCorr(m0_3level)

var_country <- as.numeric(vc$country_code[1, 1])
var_region  <- as.numeric(vc$region_id[1, 1])
var_ind     <- attr(vc, "sc")^2

var_total <- var_country + var_region + var_ind
performance::icc(m0_3level)

# ----------------------------------------------------------
# 4) Variance decomposition table
# ----------------------------------------------------------
variance_table_null <- tibble(
  Level = c("Country", "Region (within country)", "Residual (individual)"),
  Variance = c(var_country, var_region, var_ind),
  VPC = c(
    var_country / var_total,
    var_region  / var_total,
    NA_real_
  )
)

# Round values for presentation
variance_table_null <- variance_table_null %>%
  mutate(
    Variance = round(Variance, 3),
    VPC = round(VPC, 3)
  )

# ----------------------------------------------------------
# 5) Save objects for reporting
# ----------------------------------------------------------
saveRDS(m0_3level,      "Outputs/m0_3level.rds")
saveRDS(variance_table_null, "Outputs/variance_table_null.rds")

message("Empty three-level model estimated and variance decomposition saved in Outputs/.")

