## Climate Risk and Stock Market Volatility — Baseline Estimation

# install.packages(c("fixest", "broom"))
library(tidyverse)
library(lubridate)
library(fixest)
library(broom)

project_root <- path.expand("~/Documents/Stage/Stage 3 IES/Climate Risk")
final_dir    <- file.path(project_root, "data", "final")
output_dir   <- file.path(project_root, "output")
tables_dir   <- file.path(output_dir, "tables")
figures_dir  <- file.path(output_dir, "figures")
dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
setwd(project_root)

panel <- read_csv(file.path(final_dir, "panel_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = make_date(year, month, 1),
         ym   = as.factor(paste0(year, "-", sprintf("%02d", month))),  # common time FE (lambda_t)
         country = as.factor(country))

message("Loaded panel: ", nrow(panel), " country-month observations, ",
        n_distinct(panel$country), " countries.")

## SPECIFICATION
## log_realized_vol_{i,t} = beta * ClimateShock_{i,t} + gamma * Controls_{i,t}
##                            + alpha_i + lambda_t + epsilon_{i,t}
## alpha_i    = country fixed effects (time-invariant differences: geography,
##              institutions, market size, long-run climate exposure)
## lambda_t   = common time (year-month) fixed effects (global financial
##              stress, recessions, pandemics, monetary-policy shocks,
##              worldwide swings in investor risk aversion)
## beta identified from WITHIN-COUNTRY changes in climate-shock exposure,
## net of common shocks — per the plan's identification strategy.
##
## Y is log_realized_vol (not the level) — realized volatility is right-
## skewed and strictly positive; logging is standard practice in this
## literature (Bonato et al. 2023; Zhou & Ma 2025) and makes coefficients
## interpretable as approximate % effects.
##
## SE: primary = clustered by country. NOTE the caveat explicitly: with
## only 6 clusters, cluster-robust SE can understate uncertainty (Bertrand,
## Duflo & Mullainathan 2004 finger-in-the-ear rule of thumb suggests >20
## clusters is comfortable). Driscoll-Kraay SE (robust to cross-sectional
## dependence AND serial correlation, and designed for small-N/large-T
## panels like this one) are reported side by side as the more defensible
## choice given only 6 countries — flag this explicitly in the paper's
## methodology section rather than picking one silently.

controls <- c("vix", "oil_price", "us_10y_yield", "fx_depreciation_pct",
              "gdp_growth", "inflation", "credit_gdp")

## MODEL 1 — Baseline, no controls: disaster dummy (H1)
m1 <- feols(log_realized_vol ~ disaster_dummy | country + ym,
            data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 2 — Baseline + controls (H1, preferred specification)
f2 <- as.formula(paste("log_realized_vol ~ disaster_dummy +",
                        paste(controls, collapse = " + "), "| country + ym"))
m2 <- feols(f2, data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 3 — Event count instead of dummy (intensive margin, H1 robustness)
f3 <- as.formula(paste("log_realized_vol ~ disaster_count +",
                        paste(controls, collapse = " + "), "| country + ym"))
m3 <- feols(f3, data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 4 — Severity measures (H2: more severe shocks -> larger effect)
f4 <- as.formula(paste("log_realized_vol ~ people_affected_pct_pop + damage_pct_gdp +",
                        paste(controls, collapse = " + "), "| country + ym"))
m4 <- feols(f4, data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 5 — Disaster type breakdown (H4: sudden- vs slow-onset events)
f5 <- as.formula(paste("log_realized_vol ~ n_flood + n_storm + n_drought + n_wildfire +",
                        "n_extreme_temp + n_mass_movement_wet +",
                        paste(controls, collapse = " + "), "| country + ym"))
m5 <- feols(f5, data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 6 — Robustness: excludes "mass movement (wet)" from the shock
f6 <- as.formula(paste("log_realized_vol ~ disaster_dummy_noMMW +",
                        paste(controls, collapse = " + "), "| country + ym"))
m6 <- feols(f6, data = panel, vcov = "DK", panel.id = ~country + ym)

## MODEL 2b — same as Model 2 but with country-clustered SE instead of
## Driscoll-Kraay, shown side by side so the SE choice is transparent
## rather than hidden
m2_cluster <- feols(f2, data = panel, cluster = ~country)

## HETEROGENEITY (H3): country-specific disaster effect
## Interacts the disaster dummy with country, giving a separate coefficient
## per country instead of one pooled beta. This is a FIRST PASS at H3 using
## only the countries themselves as the moderator (since no external
## vulnerability/institutional-quality index has been merged in yet) — if
## time permits, a continuous moderator (e.g. World Bank governance
## indicators, insurance penetration) would be a stronger test.
f_het <- as.formula(paste("log_realized_vol ~ i(country, disaster_dummy, ref = 'India') +",
                           paste(controls, collapse = " + "), "| country + ym"))
m_het <- feols(f_het, data = panel, vcov = "DK", panel.id = ~country + ym)

## EXPORT REGRESSION TABLES (fixest::etable — no extra dependencies)

main_table <- etable(m1, m2, m3, m4, m5, m6,
                      headers = c("Dummy, no controls", "Dummy (baseline)", "Event count",
                                  "Severity (H2)", "Disaster type (H4)", "Excl. mass movement"),
                      se.below = TRUE, digits = 4, fitstat = ~ n + r2 + wr2)
print(main_table)
capture.output(main_table, file = file.path(tables_dir, "table5_baseline_regressions.txt"))

se_comparison_table <- etable(m2, m2_cluster,
                               headers = c("Driscoll-Kraay SE", "Country-clustered SE"),
                               se.below = TRUE, digits = 4)
print(se_comparison_table)
capture.output(se_comparison_table, file = file.path(tables_dir, "table6_se_comparison.txt"))

het_table <- etable(m_het, se.below = TRUE, digits = 4)
print(het_table)
capture.output(het_table, file = file.path(tables_dir, "table7_country_heterogeneity.txt"))

save_coefs <- function(model, name) {
  broom::tidy(model, conf.int = TRUE) %>%
    write_csv(file.path(tables_dir, paste0("coefs_", name, ".csv")))
}
walk2(list(m1, m2, m3, m4, m5, m6, m2_cluster, m_het),
      c("m1_baseline_nocontrols", "m2_baseline", "m3_eventcount",
        "m4_severity_H2", "m5_disastertype_H4", "m6_excl_massmovement",
        "m2_clustered_se", "het_H3"),
      save_coefs)

## DIAGNOSTICS
## D1. Residuals vs fitted (preferred model M2) — visual check for
## nonlinearity / heteroskedasticity patterns not picked up by robust SE
diag_df <- panel %>%
  filter(!is.na(log_realized_vol), !is.na(disaster_dummy),
         if_all(all_of(controls), ~ !is.na(.x))) %>%
  mutate(fitted = fitted(m2), resid = resid(m2))

fig5 <- ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Residuals vs. Fitted Values — Model 2 (baseline + controls)",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 11)

ggsave(file.path(figures_dir, "fig5_residuals_vs_fitted.png"), fig5,
       width = 8, height = 6, dpi = 300)
message("Figure 5 (residual diagnostics) saved.")

## D2. Multicollinearity check among controls (VIF via auxiliary OLS)
vif_formula <- as.formula(paste("log_realized_vol ~ disaster_dummy +", paste(controls, collapse = " + ")))
aux_lm <- lm(vif_formula, data = panel)
if (requireNamespace("car", quietly = TRUE)) {
  vif_vals <- car::vif(aux_lm)
  print(vif_vals)
  write.csv(as.data.frame(vif_vals), file.path(tables_dir, "table8_vif.csv"))
} else {
  message("Package 'car' not installed — skipping formal VIF table. ",
          "Install with install.packages('car') if you want it; ",
          "Table 4's correlation matrix from 03_descriptive_stats.R already ",
          "flags any concerning pairwise correlations among controls.")
}

## D3. Model fit summary across specifications
fit_summary <- tibble(
  model = c("M1: dummy, no controls", "M2: dummy + controls (baseline)",
            "M3: event count", "M4: severity (H2)", "M5: disaster type (H4)",
            "M6: excl. mass movement"),
  n_obs = c(m1$nobs, m2$nobs, m3$nobs, m4$nobs, m5$nobs, m6$nobs),
  within_r2 = c(r2(m1, "wr2"), r2(m2, "wr2"), r2(m3, "wr2"),
                r2(m4, "wr2"), r2(m5, "wr2"), r2(m6, "wr2"))
)
write_csv(fit_summary, file.path(tables_dir, "table9_model_fit_summary.csv"))
print(fit_summary)

## DONE
message("======================================================")
message("Baseline estimation complete.")
message("Regression tables saved to: ", tables_dir)
message("Diagnostic figure saved to: ", figures_dir)
message("======================================================")
message("\nHeadline check for H1: coefficient on disaster_dummy in Model 2:")
print(coeftable(m2)["disaster_dummy", ])
