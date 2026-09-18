## Climate Risk and Stock Market Volatility — Descriptive Statistics


library(tidyverse)
library(lubridate)

## PROJECT PATHS (same as 01_ and 02_)
project_root <- path.expand("~/Documents/Stage/Stage 3 IES/Climate Risk")
final_dir    <- file.path(project_root, "data", "final")
output_dir   <- file.path(project_root, "output")
tables_dir   <- file.path(output_dir, "tables")
figures_dir  <- file.path(output_dir, "figures")
dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
setwd(project_root)

panel <- read_csv(file.path(final_dir, "panel_monthly.csv"), show_col_types = FALSE) %>%
  mutate(date = make_date(year, month, 1))

message("Loaded panel: ", nrow(panel), " rows, ", ncol(panel), " columns, ",
        n_distinct(panel$country), " countries.")

## TABLE 1 — Pooled summary statistics (for the paper's Data section)

vars_table1 <- tribble(
  ~variable,                  ~label,
  "realized_vol",             "Monthly realized volatility (Y)",
  "log_realized_vol",         "Log realized volatility",
  "disaster_dummy",           "Climate disaster dummy (0/1)",
  "disaster_count",           "Number of climate disasters",
  "people_affected_pct_pop",  "People affected (% of population)",
  "damage_pct_gdp",           "Economic damage (% of GDP)",
  "total_deaths",             "Deaths from climate disasters",
  "vix",                      "VIX (global risk aversion)",
  "oil_price",                "Oil price (WTI, USD/barrel)",
  "us_10y_yield",             "US 10-year Treasury yield (%)",
  "fx_depreciation_pct",      "FX depreciation vs USD (%, month-on-month)",
  "gdp_growth",                "GDP growth (%, annual)",
  "inflation",                 "Inflation (%, annual)",
  "credit_gdp",                "Domestic credit to private sector (% GDP)"
)

summarise_var <- function(x) {
  x <- x[!is.na(x)]
  tibble(
    obs    = length(x),
    mean   = mean(x),
    sd     = sd(x),
    min    = min(x),
    median = median(x),
    max    = max(x)
  )
}

table1 <- vars_table1 %>%
  mutate(stats = map(variable, ~ summarise_var(panel[[.x]]))) %>%
  unnest(stats) %>%
  mutate(across(c(mean, sd, min, median, max), ~ round(.x, 4)))

write_csv(table1, file.path(tables_dir, "table1_summary_statistics.csv"))
message("Table 1 (pooled summary statistics) saved.")
print(table1, n = Inf)

## TABLE 2 — By-country summary (Y and climate-shock exposure)

table2 <- panel %>%
  group_by(country) %>%
  summarise(
    n_months               = n(),
    first_month            = min(date),
    last_month             = max(date),
    mean_realized_vol      = mean(realized_vol, na.rm = TRUE),
    sd_realized_vol        = sd(realized_vol, na.rm = TRUE),
    pct_months_with_disaster = 100 * mean(disaster_dummy, na.rm = TRUE),
    total_disasters         = sum(disaster_count, na.rm = TRUE),
    mean_damage_pct_gdp     = mean(damage_pct_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(table2, file.path(tables_dir, "table2_by_country_summary.csv"))
message("Table 2 (by-country summary) saved.")
print(table2, n = Inf)

## TABLE 3 — Disaster type breakdown by country (for H4 motivation)

table3 <- panel %>%
  group_by(country) %>%
  summarise(
    flood         = sum(n_flood, na.rm = TRUE),
    storm         = sum(n_storm, na.rm = TRUE),
    drought       = sum(n_drought, na.rm = TRUE),
    wildfire      = sum(n_wildfire, na.rm = TRUE),
    extreme_temp  = sum(n_extreme_temp, na.rm = TRUE),
    mass_movement_wet = sum(n_mass_movement_wet, na.rm = TRUE),
    total         = flood + storm + drought + wildfire + extreme_temp + mass_movement_wet,
    .groups = "drop"
  )

write_csv(table3, file.path(tables_dir, "table3_disaster_types_by_country.csv"))
message("Table 3 (disaster type breakdown) saved.")
print(table3, n = Inf)

## TABLE 4 — Correlation matrix (Y, X, controls)

cor_vars <- c("realized_vol", "log_realized_vol", "disaster_dummy", "disaster_count",
              "people_affected_pct_pop", "damage_pct_gdp", "vix", "oil_price",
              "us_10y_yield", "fx_depreciation_pct", "gdp_growth", "inflation", "credit_gdp")

cor_matrix <- panel %>%
  select(all_of(cor_vars)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

write.csv(cor_matrix, file.path(tables_dir, "table4_correlation_matrix.csv"))
message("Table 4 (correlation matrix) saved.")
print(cor_matrix)

# Quick early look at the core relationship: correlation between realized
# vol and each disaster/severity variable specifically (H1 preview)
h1_preview <- cor_matrix["realized_vol", c("disaster_dummy", "disaster_count",
                                             "people_affected_pct_pop", "damage_pct_gdp")]
message("\nH1 preview — raw correlation of realized volatility with climate-shock variables:")
print(h1_preview)
message("(This is NOT the regression result — no fixed effects or controls yet — just a first look.)")

## FIGURE 1 — Realized volatility over time, by country

fig1 <- ggplot(panel, aes(x = date, y = realized_vol)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ country, scales = "free_y", ncol = 2) +
  labs(title = "Monthly Realized Stock-Market Volatility by Country",
       subtitle = "2010-2024 (South Africa begins 2012 — see Data section note)",
       x = NULL, y = "Realized volatility") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(figures_dir, "fig1_realized_volatility_by_country.png"),
       fig1, width = 10, height = 7, dpi = 300)
message("Figure 1 saved.")

## FIGURE 2 — Climate disasters over time, stacked by type (pooled, all countries)

disaster_by_year_type <- panel %>%
  group_by(year) %>%
  summarise(
    Flood = sum(n_flood, na.rm = TRUE),
    Storm = sum(n_storm, na.rm = TRUE),
    Drought = sum(n_drought, na.rm = TRUE),
    Wildfire = sum(n_wildfire, na.rm = TRUE),
    `Extreme temperature` = sum(n_extreme_temp, na.rm = TRUE),
    `Mass movement (wet)` = sum(n_mass_movement_wet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "disaster_type", values_to = "count")

fig2 <- ggplot(disaster_by_year_type, aes(x = year, y = count, fill = disaster_type)) +
  geom_col() +
  labs(title = "Physical Climate Disasters by Year and Type (all 6 countries pooled)",
       x = NULL, y = "Number of disasters", fill = "Disaster type") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(figures_dir, "fig2_disasters_by_year_type.png"),
       fig2, width = 9, height = 6, dpi = 300)
message("Figure 2 saved.")


## FIGURE 2b — Climate disasters over time, stacked by type, faceted by country
## (companion to FIGURE 2, which pools all six countries)

disaster_by_year_type_country <- panel %>%
  group_by(country, year) %>%
  summarise(
    Flood = sum(n_flood, na.rm = TRUE),
    Storm = sum(n_storm, na.rm = TRUE),
    Drought = sum(n_drought, na.rm = TRUE),
    Wildfire = sum(n_wildfire, na.rm = TRUE),
    `Extreme temperature` = sum(n_extreme_temp, na.rm = TRUE),
    `Mass movement (wet)` = sum(n_mass_movement_wet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(country, year), names_to = "disaster_type", values_to = "count") %>%
  mutate(country = recode(country, "SouthAfrica" = "South Africa"))

fig2b <- ggplot(disaster_by_year_type_country, aes(x = year, y = count, fill = disaster_type)) +
  geom_col() +
  facet_wrap(~ country, scales = "free_y", ncol = 2) +
  labs(title = "Physical Climate Disasters by Year and Type, by Country",
       x = NULL, y = "Number of disasters", fill = "Disaster type") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave(file.path(figures_dir, "fig2b_disasters_by_year_type_by_country.png"),
       fig2b, width = 10, height = 10, dpi = 300)
message("Figure 2b saved.")

## FIGURE 3 — Realized volatility: disaster months vs. non-disaster months
## (preliminary visual test of H1, pooled and by country)

fig3 <- panel %>%
  mutate(disaster_label = if_else(disaster_dummy == 1, "Disaster month", "No disaster")) %>%
  ggplot(aes(x = disaster_label, y = realized_vol, fill = disaster_label)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  labs(title = "Realized Volatility: Disaster Months vs. Non-Disaster Months",
       subtitle = "Preliminary visual comparison — not a regression result",
       x = NULL, y = "Realized volatility", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

ggsave(file.path(figures_dir, "fig3_volatility_by_disaster_status.png"),
       fig3, width = 10, height = 7, dpi = 300)
message("Figure 3 saved.")

## FIGURE 4 — Correlation heatmap

cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  mutate(var1 = factor(var1, levels = cor_vars),
         var2 = factor(var2, levels = cor_vars))

fig4 <- ggplot(cor_long, aes(x = var2, y = var1, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 2.6) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                        limits = c(-1, 1)) +
  labs(title = "Correlation Matrix — Volatility, Climate Shocks, and Controls",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(figures_dir, "fig4_correlation_heatmap.png"),
       fig4, width = 9, height = 8, dpi = 300)
message("Figure 4 saved.")

## DONE
message("======================================================")
message("Descriptive analysis complete.")
message("Tables saved to: ", tables_dir)
message("Figures saved to: ", figures_dir)
message("======================================================")
