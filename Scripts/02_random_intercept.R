############################################################
# 02_random_intercept_models.R
# Random intercept models with country-level intercept
############################################################

library(lme4)
library(dplyr)
library(ggplot2)
library(tibble)

# ----------------------------------------------------------
# 1) Load BASE dataset (no income / gini)
# ----------------------------------------------------------
wvs_clean_base <- readRDS("Data/wvs_clean_base.rds")

############################################################
# 1) Are age and gender associated with life satisfaction?
#    Is the effect of age linear?
#    Is country-level variance explained (and why)?
############################################################

# ----------------------------------------------------------
# 1.0 Empty country-level model (for variance comparison)
# ----------------------------------------------------------
m0_country <- lmer(
  life_satisfaction ~ 1 + (1 | country_code),
  data = wvs_clean_base,
  REML = TRUE
)

# ----------------------------------------------------------
# 1.1 Random intercept model: age (linear) + female
# ----------------------------------------------------------
m1_age_lin <- lmer(
  life_satisfaction ~ age_c + female + (1 | country_code),
  data = wvs_clean_base,
  REML = TRUE
)

# Extract fixed-effects coefficients and add significance stars

coef_table_age <- summary(m1_age_lin)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  mutate(
    # Add significance stars based on t-values (normal approximation)
    stars = case_when(
      abs(`t value`) >= 2.58 ~ "***",  # p < 0.01
      abs(`t value`) >= 1.96 ~ "**",   # p < 0.05
      abs(`t value`) >= 1.64 ~ "*",    # p < 0.10
      TRUE ~ ""
    )
  )

coef_table_age

# ----------------------------------------------------------
# 1.2 Test non-linearity of age (quadratic term)
# ----------------------------------------------------------
m2_age_quad <- lmer(
  life_satisfaction ~ age_c + age_c2 + female + (1 | country_code),
  data = wvs_clean_base,
  REML = TRUE
)

# Extract fixed-effects coefficients and add significance stars
coef_table_age_sq <- summary(m2_age_quad)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  mutate(
    # Add significance stars based on t-values (normal approximation)
    stars = case_when(
      abs(`t value`) >= 2.58 ~ "***",  # p < 0.01
      abs(`t value`) >= 1.96 ~ "**",   # p < 0.05
      abs(`t value`) >= 1.64 ~ "*",    # p < 0.10
      TRUE ~ ""
    )
  )

coef_table_age_sq

# Likelihood ratio test (use ML for fixed-effects comparison)
anova(
  update(m1_age_lin, REML = FALSE),
  update(m2_age_quad, REML = FALSE)
)

summary(m2_age_quad)

# ----------------------------------------------------------
# 1.3 Does age + gender explain country-level variance?
# ----------------------------------------------------------
#Extract the variance terms infrom the fitted models
vc0 <- VarCorr(m0_country)
vc2 <- VarCorr(m2_age_quad)

#Retrieve the country-level random-intercept variance from each model 
var_country_0 <- as.numeric(vc0$country_code[1, 1]) #variance between countries (empty model)
var_country_2 <- as.numeric(vc2$country_code[1, 1]) #variance between countries (adjusted model)

perc_reduction <- (var_country_0 - var_country_2) / var_country_0 * 100

cat("Country-level variance (empty model):", round(var_country_0, 4), "\n")
cat("Country-level variance (age + gender):", round(var_country_2, 4), "\n")
cat("Percent reduction in country variance:", round(perc_reduction, 2), "%\n\n")

############################################################
# 2) Country ranking controlling for age and gender
############################################################

# ----------------------------------------------------------
# 2.1 Final model for ranking
# ----------------------------------------------------------
m_final <- m2_age_quad

# ----------------------------------------------------------
# 2.2 Extract country random intercepts (BLUPs) + ISO labels
# ----------------------------------------------------------
# Country code to ISO label mapping
country_labels <- wvs_clean_base %>%
  distinct(country_code, country_iso)

re_country <- ranef(m_final)$country_code

country_table <- tibble(
  country_code = rownames(re_country),
  intercept    = re_country[, 1]
) %>%
  left_join(country_labels, by = "country_code") %>%
  arrange(desc(intercept))

# ----------------------------------------------------------
# 2.3 Graph: country ranking
# ----------------------------------------------------------
# Keep only top 15 and bottom 15 countries for the plot
# Extract country random intercepts (BLUPs) + conditional variances

re_country <- ranef(m_final, condVar = TRUE)$country_code

country_table_ci <- tibble(
  country_code = rownames(re_country),
  intercept    = re_country[, 1],
  se           = sqrt(attr(re_country, "postVar")[1, 1, ])
) %>%
  mutate(
    ci_low  = intercept - 1.96 * se,
    ci_high = intercept + 1.96 * se
  ) %>%
  left_join(country_labels, by = "country_code") %>%
  arrange(desc(intercept))

# Keep only top 15 and bottom 15 countries for the plot
plot_countries <- country_table_ci %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 15 | rank > n() - 15)

# Plot
country_ranking <- ggplot(plot_countries,
       aes(x = reorder(country_iso, intercept),
           y = intercept)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, color = "grey40") +
  geom_point(size = 2.2, shape = 21, fill = "steelblue",
             color = "grey20", stroke = 0.3) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Country random intercept (BLUP)\n(adjusted life satisfaction)",
    title = "Country ranking in life satisfaction (top/bottom 15)",
    subtitle = "Multilevel model controlling for age and gender"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )
# ----------------------------------------------------------
# 3) Save objects for reporting
# ----------------------------------------------------------

saveRDS(m0_country,   "Outputs/m0_country.rds")
saveRDS(m1_age_lin,   "Outputs/m1_age_lin.rds")
saveRDS(m2_age_quad,  "Outputs/m2_age_quad.rds")
saveRDS(coef_table_age, "Outputs/coef_table_age.rds")
saveRDS(coef_table_age_sq, "Outputs/coef_table_age_sq.rds")



# Country ranking tables
saveRDS(country_table_ci,   "Outputs/country_ranking_table.rds")
saveRDS(plot_countries,  "Outputs/country_ranking_plotdata.rds")
ggsave("Outputs/country_ranking_topbottom15.png", country_ranking, width = 7, height = 6, dpi = 300)

message("Objects saved in Outputs/.")
