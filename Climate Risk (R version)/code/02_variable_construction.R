## Climate Risk and Stock Market Volatility — Variable Construction


library(tidyverse)
library(lubridate)
library(readxl)
library(WDI)

project_root <- path.expand("~/Documents/Stage/Stage 3 IES/Climate Risk")
raw_dir      <- file.path(project_root, "data", "raw")
final_dir    <- file.path(project_root, "data", "final")
setwd(project_root)

countries <- c("India", "Philippines", "Indonesia", "Brazil", "Mexico", "SouthAfrica")
country_display <- c(India = "India", Philippines = "Philippines", Indonesia = "Indonesia",
                      Brazil = "Brazil", Mexico = "Mexico", SouthAfrica = "South Africa")
country_iso3 <- c(India = "IND", Philippines = "PHL", Indonesia = "IDN",
                   Brazil = "BRA", Mexico = "MEX", SouthAfrica = "ZAF")

sample_start <- as.Date("2010-01-01")
sample_end   <- as.Date("2024-12-31")

## PART A — OUTCOME VARIABLE: monthly realized stock-market volatility

compute_monthly_rv <- function(nm) {
  df <- read_csv(file.path(raw_dir, paste0("stock_", nm, ".csv")), show_col_types = FALSE)
  df <- df %>%
    mutate(date = as.Date(date),
           price = coalesce(adjusted, close)) %>%     # adjusted close preferred, fall back to close
    filter(!is.na(price), date >= sample_start, date <= sample_end) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)

  df <- df %>%
    mutate(log_price = log(price),
           log_return = log_price - lag(log_price)) %>%
    filter(!is.na(log_return))

  monthly <- df %>%
    mutate(year = year(date), month = month(date)) %>%
    group_by(year, month) %>%
    summarise(
      n_trading_days   = n(),
      realized_vol     = sqrt(sum(log_return^2, na.rm = TRUE)),
      mean_return      = mean(log_return, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(country = nm,
           log_realized_vol = log(realized_vol))

  monthly
}

rv_all <- map_dfr(countries, compute_monthly_rv)

# Sanity flag: months with very few trading days (e.g. index outages, partial
# months at the sample edges) make realized vol noisy/unreliable.
low_coverage <- rv_all %>% filter(n_trading_days < 10)
if (nrow(low_coverage) > 0) {
  message(nrow(low_coverage), " country-months have < 10 trading days — inspect before treating them as reliable RV estimates:")
  print(low_coverage %>% select(country, year, month, n_trading_days))
}

write_csv(rv_all, file.path(final_dir, "volatility_monthly.csv"))
message("Part A done: ", nrow(rv_all), " country-month volatility observations.")

## PART B — MAIN EXPLANATORY VARIABLE: physical climate-shock exposure (EM-DAT)

emdat <- read_excel(file.path(raw_dir, "emdat_raw.xlsx"), sheet = "EM-DAT Data")

# B1. Filter to physical/climate-related disasters only 
# EM-DAT's "Natural" group includes Geophysical (earthquake, volcanic
# activity) and Biological (epidemic) events, which are not physical climate
# risk as defined in this paper (Step 1: floods, storms, droughts, wildfires,
# extreme temperatures).
climate_subgroups <- c("Meteorological", "Hydrological", "Climatological")

emdat_climate <- emdat %>%
  filter(`Disaster Subgroup` %in% climate_subgroups) %>%
  filter(`Start Year` >= 2010, `Start Year` <= 2024) %>%   # drop 2025-2026 records outside sample
  mutate(
    start_day_imputed = is.na(`Start Day`),
    start_date = make_date(`Start Year`, `Start Month`, coalesce(`Start Day`, 1)),
    country = recode(Country,
                      "India" = "India", "Philippines" = "Philippines",
                      "Indonesia" = "Indonesia", "Brazil" = "Brazil",
                      "Mexico" = "Mexico", "South Africa" = "SouthAfrica"),
    disaster_type_clean = case_when(
      `Disaster Type` == "Flood" ~ "flood",
      `Disaster Type` == "Storm" ~ "storm",
      `Disaster Type` == "Drought" ~ "drought",
      `Disaster Type` == "Wildfire" ~ "wildfire",
      `Disaster Type` == "Extreme temperature" ~ "extreme_temp",
      `Disaster Type` == "Mass movement (wet)" ~ "mass_movement_wet",
      `Disaster Type` == "Glacial lake outburst flood" ~ "glof",
      TRUE ~ "other_climate"
    ),
    year = `Start Year`,
    month = `Start Month`
  )

message("EM-DAT: ", nrow(emdat), " raw records -> ", nrow(emdat_climate),
        " after keeping only Meteorological/Hydrological/Climatological and 2010-2024.")
message("  (", sum(emdat_climate$start_day_imputed), " records had no Start Day; imputed to day 1 for date construction — this only affects which day-of-month is assigned, not the year-month used for aggregation.)")

# B2. Aggregate to country-month 

disaster_monthly <- emdat_climate %>%
  group_by(country, year, month) %>%
  summarise(
    disaster_count        = n(),
    disaster_count_noMMW   = sum(disaster_type_clean != "mass_movement_wet"),  # robustness: excludes mass movement (wet)
    people_affected        = sum(`Total Affected`, na.rm = TRUE),
    economic_damage_000usd = sum(`Total Damage, Adjusted ('000 US$)`, na.rm = TRUE),
    total_deaths            = sum(`Total Deaths`, na.rm = TRUE),
    n_flood          = sum(disaster_type_clean == "flood"),
    n_storm          = sum(disaster_type_clean == "storm"),
    n_drought        = sum(disaster_type_clean == "drought"),
    n_wildfire       = sum(disaster_type_clean == "wildfire"),
    n_extreme_temp   = sum(disaster_type_clean == "extreme_temp"),
    n_mass_movement_wet = sum(disaster_type_clean == "mass_movement_wet"),
    .groups = "drop"
  ) %>%
  mutate(disaster_dummy = as.integer(disaster_count > 0),
         disaster_dummy_noMMW = as.integer(disaster_count_noMMW > 0))

# B3. Build the FULL country-month grid and fill non-disaster months with zero 
# This step matters: EM-DAT only has rows for months with a disaster. Every
# country-month with no recorded disaster must be an explicit zero, not a
# missing row, otherwise the panel silently drops your "no shock" observations.
full_grid <- expand_grid(country = countries,
                          year = 2010:2024,
                          month = 1:12) %>%
  filter(make_date(year, month, 1) <= sample_end)

disaster_panel <- full_grid %>%
  left_join(disaster_monthly, by = c("country", "year", "month")) %>%
  mutate(across(c(disaster_count, disaster_count_noMMW, people_affected, economic_damage_000usd,
                   total_deaths, n_flood, n_storm, n_drought, n_wildfire, n_extreme_temp,
                   n_mass_movement_wet, disaster_dummy, disaster_dummy_noMMW),
                ~ replace_na(.x, 0)))

write_csv(disaster_panel, file.path(final_dir, "disasters_monthly.csv"))
message("Part B done: ", nrow(disaster_panel), " country-month disaster observations (",
        sum(disaster_panel$disaster_dummy), " months with at least one climate disaster).")

## PART C — SEVERITY NORMALIZATION: population and GDP (for severity ratios)

wdi_levels <- WDI(country = country_iso3,
                   indicator = c(population = "SP.POP.TOTL",
                                  gdp_current_usd = "NY.GDP.MKTP.CD"),
                   start = 2010, end = 2024) %>%
  rename(iso3 = iso3c) %>%
  mutate(country = names(country_iso3)[match(iso3, country_iso3)]) %>%
  select(country, year, population, gdp_current_usd)

write_csv(wdi_levels, file.path(raw_dir, "wdi_levels.csv"))

## PART D — GLOBAL & COUNTRY-LEVEL CONTROL VARIABLES, aggregated to monthly

#  D1. Global controls: VIX, oil (WTI), US 10Y yield -> monthly average ---

read_global <- function(fname, valcol) {
  df <- read_csv(file.path(raw_dir, fname), show_col_types = FALSE)
  names(df) <- c("date", "open", "high", "low", "close", "volume", "adjusted")[seq_len(ncol(df))]
  df %>%
    mutate(date = as.Date(date), year = year(date), month = month(date)) %>%
    filter(!is.na(close)) %>%
    group_by(year, month) %>%
    summarise(!!valcol := mean(close, na.rm = TRUE), .groups = "drop")
}

vix_m    <- read_global("global_VIX.csv", "vix")
wti_m    <- read_global("global_WTI.csv", "oil_price")
ust10y_m <- read_global("global_UST10Y.csv", "us_10y_yield")

global_controls <- vix_m %>%
  full_join(wti_m, by = c("year", "month")) %>%
  full_join(ust10y_m, by = c("year", "month"))

# D2. FX rates vs USD -> monthly average level + monthly % depreciation 
read_fx <- function(nm) {
  df <- read_csv(file.path(raw_dir, paste0("fx_", nm, ".csv")), show_col_types = FALSE)
  names(df) <- c("date", "open", "high", "low", "close", "volume", "adjusted", "country")[seq_len(ncol(df))]
  df %>%
    mutate(date = as.Date(date), year = year(date), month = month(date)) %>%
    filter(!is.na(close)) %>%
    group_by(country, year, month) %>%
    summarise(fx_rate = mean(close, na.rm = TRUE), .groups = "drop")
}

fx_monthly <- map_dfr(countries, read_fx) %>%
  arrange(country, year, month) %>%
  group_by(country) %>%
  mutate(fx_depreciation_pct = (fx_rate / lag(fx_rate) - 1) * 100) %>%
  ungroup()

# D3. Annual WDI macro controls (GDP growth, inflation, credit/GDP) 
wdi_macro <- read_csv(file.path(raw_dir, "wdi_macro.csv"), show_col_types = FALSE) %>%
  rename(iso3 = iso3c) %>%
  mutate(country = names(country_iso3)[match(iso3, country_iso3)]) %>%
  select(country, year, gdp_growth, inflation, credit_gdp) %>%
  filter(!is.na(country))

wdi_macro_monthly <- wdi_macro %>%
  crossing(month = 1:12) %>%
  select(country, year, month, gdp_growth, inflation, credit_gdp)

## PART E — MERGING EVERYTHING INTO THE FINAL COUNTRY-MONTH PANEL

panel <- rv_all %>%
  select(country, year, month, n_trading_days, realized_vol, log_realized_vol, mean_return) %>%
  left_join(disaster_panel, by = c("country", "year", "month")) %>%
  left_join(global_controls, by = c("year", "month")) %>%
  left_join(fx_monthly, by = c("country", "year", "month")) %>%
  left_join(wdi_levels, by = c("country", "year")) %>%
  left_join(wdi_macro_monthly, by = c("country", "year", "month")) %>%
  mutate(
    people_affected_pct_pop = 100 * people_affected / population,
    damage_pct_gdp          = 100 * (economic_damage_000usd * 1000) / gdp_current_usd
  ) %>%
  arrange(country, year, month)

write_csv(panel, file.path(final_dir, "panel_monthly.csv"))

message("======================================================")
message("DONE. Final panel: ", nrow(panel), " country-month observations, ",
        ncol(panel), " variables.")
message("Saved to: ", file.path(final_dir, "panel_monthly.csv"))
message("======================================================")

# Quick completeness check — flag any variable with unexpectedly high missingness
missing_report <- panel %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))
print(missing_report)
