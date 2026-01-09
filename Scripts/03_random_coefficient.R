############################################################
# 03_random_slope_models.R
# Random coefficient models:
# Income and life satisfaction
############################################################

library(lme4)
library(dplyr)
library(performance)
library(ggplot2)
library(ggeffects)
library(tibble)

# ----------------------------------------------------------
# Load cleaned data
# ----------------------------------------------------------
wvs_clean <- readRDS("Data/wvs_clean_income.rds")

#-----------------------------------------------------------
# 1) Relationship between income and life satisfaction
#    Does the income effect vary across countries?
#----------------------------------------------------------

# ----------------------------------------------------------
# 1.1 Random intercept model (baseline)
# y_ic = β0 + β1*income_c + β2*age_c + β3*age_c2 + β4*female
#        + u0c + e_ic
# ----------------------------------------------------------
m0_income <- lmer(
  life_satisfaction ~ income_c + age_c + age_c2 + female +
    (1 | country_code),
  data = wvs_clean
)

summary(m0_income)
icc(m0_income)

# ----------------------------------------------------------
# 1.2 Random coefficient model for income
# y_ic = β0 + β1*income_c + ... + u0c + u1c*income_c + e_ic
# ----------------------------------------------------------
m1_income_rs <- lmer(
  life_satisfaction ~ income_c + age_c + age_c2 + female +
    (1 + income_c | country_code),
  data = wvs_clean
)

summary(m1_income_rs)

# ----------------------------------------------------------
# 1.3 Does the income effect vary across countries?
# ----------------------------------------------------------
anova(
  update(m0_income, REML = FALSE),
  update(m1_income_rs, REML = FALSE)
)

VarCorr(m1_income_rs)

#-------------------------------------------------------------
# 2) Does income inequality (Gini) moderate the income effect?
#-------------------------------------------------------------

# ----------------------------------------------------------
# 2.1 Cross-level interaction: income × gini
# ----------------------------------------------------------
m2_income_gini <- lmer(
  life_satisfaction ~ income_c * gini_c + age_c + age_c2 + female +
    (1 + income_c | country_code),
  data = wvs_clean
)

summary(m2_income_gini)

# ----------------------------------------------------------
# 2.2 Model comparison (does Gini improve fit)
# ----------------------------------------------------------
anova(
  update(m1_income_rs, REML = FALSE),
  update(m2_income_gini, REML = FALSE)
)

# Extract fixed-effects coefficients from the final model
coef_table_income_gini <- summary(m2_income_gini)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  mutate(
    stars = case_when(
      abs(`t value`) >= 2.58 ~ "***",  # p < 0.01
      abs(`t value`) >= 1.96 ~ "**",   # p < 0.05
      abs(`t value`) >= 1.64 ~ "*",    # p < 0.10
      TRUE ~ ""
    )
  )

#----------------------------------------------------------
# 3) Visualisation: income–life satisfaction by inequality
#----------------------------------------------------------

# ----------------------------------------------------------
# Predicted values at low / mean / high inequality
# ----------------------------------------------------------
gini_sd <- sd(wvs_clean$gini_c, na.rm = TRUE)

# Predictions
pred <- ggpredict(
  m2_income_gini,
  terms = c(
    "income_c",
    paste0("gini_c [", 
           round(-gini_sd, 2), ", 0, ", round(gini_sd, 2), "]")
  )
)

# Relabel inequality levels (interpretation-friendly)
pred$group <- factor(
  pred$group,
  labels = c(
    "Low inequality (−1 SD)",
    "Average inequality (mean)",
    "High inequality (+1 SD)"
  )
)

# Plot
p_income_gini <- plot(pred) +
  labs(
    x = "Income (centered)",
    y = "Predicted life satisfaction",
    colour = "Income inequality",
    title = "Predicted values of life satisfaction by income and inequality"
  ) +
  theme_minimal()

p_income_gini
# ----------------------------------------------------------
# 4) Save objects for reporting
# ----------------------------------------------------------
# Save table for reporting
saveRDS(coef_table_income_gini, "Outputs/coef_table_income_gini.rds")

#Saving plot for reporting 
ggsave("Outputs/Predicted_values_Gini_model.png", p_income_gini, width = 7, height = 6, dpi = 300)

message("Objects saved in Outputs/.")
