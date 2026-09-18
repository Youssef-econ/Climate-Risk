## Climate Risk and Stock Market Volatility, Data Collection
## Countries: India, Philippines, Indonesia, Brazil, Mexico, South Africa
## Period: 2010-01-01 to 2024-12-31

library(quantmod)
library(tidyverse)
library(readxl)
library(WDI)

project_root <- path.expand("~/Documents/Stage/Stage 3 IES/Climate Risk")
raw_dir      <- file.path(project_root, "data", "raw")
final_dir    <- file.path(project_root, "data", "final")
output_dir   <- file.path(project_root, "output")

dir.create(raw_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

setwd(project_root)

start_date <- as.Date("2010-01-01")
end_date   <- as.Date("2024-12-31")

## 1. STOCK MARKET INDICES (daily closing prices), source: Yahoo Finance

ticker_candidates <- list(
  India        = c("^BSESN"),
  Philippines  = c("PSEI.PS"),
  Indonesia    = c("^JKSE"),
  Brazil       = c("^BVSP"),
  Mexico       = c("^MXX"),
  SouthAfrica  = c("^J203.JO", "J203.L", "EZA")
)

stock_list <- list()
for (nm in names(ticker_candidates)) {
  candidates <- ticker_candidates[[nm]]
  success <- FALSE
  for (tkr in candidates) {
    message("Downloading: ", nm, " (", tkr, ")")
    result <- tryCatch({
      obj <- getSymbols(tkr, src = "yahoo",
                        from = start_date, to = end_date,
                        auto.assign = FALSE)
      df <- data.frame(date = index(obj), coredata(obj)) %>%
        rename_with(~ c("date","open","high","low","close","volume","adjusted"), everything())
      df$country <- nm
      df$ticker_used <- tkr
      df
    }, error = function(e) {
      message("  FAILED for ", tkr, ": ", e$message)
      NULL
    })
    if (!is.null(result) && nrow(result) > 0) {
      stock_list[[nm]] <- result
      write.csv(result, file.path(raw_dir, paste0("stock_", nm, ".csv")), row.names = FALSE)
      message("  OK — ", nrow(result), " rows saved for ", nm, " using ", tkr)
      success <- TRUE
      break
    }
  }
  if (!success) message("  *** ALL CANDIDATES FAILED for ", nm, " — needs manual download ***")
}

stocks_all <- bind_rows(stock_list)
write.csv(stocks_all, file.path(raw_dir, "stocks_all_countries.csv"), row.names = FALSE)

## 2. CLIMATE DISASTER DATA, source: EM-DAT

emdat_raw <- read_excel(file.path(raw_dir, "emdat_raw.xlsx"))
write.csv(emdat_raw, file.path(raw_dir, "emdat_raw.csv"), row.names = FALSE)

## 3. MACRO-FINANCIAL CONTROL VARIABLES
## 3a. Global/common controls (daily, from Yahoo Finance)
global_tickers <- c(VIX = "^VIX",           # CBOE Volatility Index
                    WTI = "CL=F",           # Crude oil (WTI front-month futures)
                    UST10Y = "^TNX")        # US 10-year Treasury yield

for (nm in names(global_tickers)) {
  message("Downloading: ", nm, " (", global_tickers[nm], ")")
  tryCatch({
    obj <- getSymbols(global_tickers[nm], src = "yahoo",
                      from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(date = index(obj), coredata(obj))
    write.csv(df, file.path(raw_dir, paste0("global_", nm, ".csv")), row.names = FALSE)
    message("  OK — ", nrow(df), " rows saved for ", nm)
  }, error = function(e) message("  FAILED for ", nm, ": ", e$message))
}

## 3b. Exchange rates vs USD (daily, Yahoo Finance FX tickers)
fx_tickers <- c(India = "USDINR=X", Philippines = "USDPHP=X",
                Indonesia = "USDIDR=X", Brazil = "USDBRL=X",
                Mexico = "USDMXN=X", SouthAfrica = "USDZAR=X")

for (nm in names(fx_tickers)) {
  message("Downloading: ", nm, " (", fx_tickers[nm], ")")
  tryCatch({
    obj <- getSymbols(fx_tickers[nm], src = "yahoo",
                      from = start_date, to = end_date, auto.assign = FALSE)
    df <- data.frame(date = index(obj), coredata(obj))
    df$country <- nm
    write.csv(df, file.path(raw_dir, paste0("fx_", nm, ".csv")), row.names = FALSE)
    message("  OK — ", nrow(df), " rows saved for ", nm)
  }, error = function(e) message("  FAILED for ", nm, ": ", e$message))
}

## 3c. Annual/quarterly macro fundamentals, World Bank via WDI package
wdi_countries <- c("IN","PH","ID","BR","MX","ZA")
macro_wdi <- WDI(country = wdi_countries,
                 indicator = c(gdp_growth = "NY.GDP.MKTP.KD.ZG",
                               inflation  = "FP.CPI.TOTL.ZG",
                               credit_gdp = "FS.AST.PRVT.GD.ZS"),
                 start = 2010, end = 2024)
write.csv(macro_wdi, file.path(raw_dir, "wdi_macro.csv"), row.names = FALSE)
