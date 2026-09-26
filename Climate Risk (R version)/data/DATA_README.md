# Data Sources and Variable Construction

## Final dataset

The main estimation dataset is:

`data/final/panel_monthly.csv`

The unit of observation is **country-month**.

Countries:
- Brazil
- India
- Indonesia
- Mexico
- Philippines
- South Africa

Sample period:
- January 2010–December 2024 for Brazil, India, Indonesia, Mexico, and the Philippines.
- February 2012–December 2024 for South Africa because the available Yahoo Finance series for the JSE All Share Index (`^J203.JO`) begins on 8 February 2012.

## Data sources

### Climate disasters

Source: **EM-DAT, International Disaster Database**

The archived file used in the analysis is:

`data/raw/emdat_raw.xlsx`

The original extraction date was not recorded during initial data collection. The archived extract contains EM-DAT records updated through at least **17 July 2026** and is retained unchanged to reproduce the reported estimates.

Included disaster subgroups:
- Meteorological
- Hydrological
- Climatological

Disasters are assigned to months using their **EM-DAT Start Year and Start Month**.

If Start Day is missing, day 1 is used only to construct a valid date. This does not affect assignment to the country-month panel.

Months without a recorded climate disaster are coded as zero rather than missing.

### Stock-market indices

Source: **Yahoo Finance**, downloaded using `quantmod::getSymbols()`.

| Country | Ticker |
|---|---|
| India | `^BSESN` |
| Philippines | `PSEI.PS` |
| Indonesia | `^JKSE` |
| Brazil | `^BVSP` |
| Mexico | `^MXX` |
| South Africa | `^J203.JO` |

The original extraction date was not logged. The archived raw files in `data/raw/` are the exact files used to generate the reported results.

### Global financial controls

Source: **Yahoo Finance**

| Variable | Ticker |
|---|---|
| VIX | `^VIX` |
| WTI crude oil | `CL=F` |
| US 10-year Treasury yield | `^TNX` |

Daily observations are converted to monthly averages of closing values.

### Exchange rates

Source: **Yahoo Finance**

| Country | Ticker |
|---|---|
| India | `USDINR=X` |
| Philippines | `USDPHP=X` |
| Indonesia | `USDIDR=X` |
| Brazil | `USDBRL=X` |
| Mexico | `USDMXN=X` |
| South Africa | `USDZAR=X` |

Daily closing exchange rates are averaged by month.

Monthly depreciation is:

`FX depreciation (%) = 100 × (FX_t / FX_(t-1) - 1)`

### Macroeconomic controls

Source: **World Bank World Development Indicators (WDI)**.

| Variable | WDI indicator |
|---|---|
| GDP growth | `NY.GDP.MKTP.KD.ZG` |
| CPI inflation | `FP.CPI.TOTL.ZG` |
| Domestic credit to private sector (% GDP) | `FS.AST.PRVT.GD.ZS` |
| Population | `SP.POP.TOTL` |
| GDP, current USD | `NY.GDP.MKTP.CD` |

GDP growth, inflation, and credit/GDP are annual variables and the corresponding annual observation is assigned to all 12 months of that year.

## Returns

Adjusted closing prices are used when available; otherwise closing prices are used.

Daily log returns are calculated as:

`r_(i,d) = ln(P_(i,d)) - ln(P_(i,d-1))`

Returns are expressed in log-return units and are not multiplied by 100.

## Monthly realized volatility

For country `i` and month `t`:

`RV_(i,t) = sqrt(sum_d r_(i,d)^2)`

where the sum is taken over all trading days within that country-month.

The main regression dependent variable is:

`log_realized_vol = ln(RV_(i,t))`

## Climate-shock measures

For each country-month:

- `disaster_dummy`: 1 if at least one climate disaster begins during the month, 0 otherwise.
- `disaster_count`: total number of disasters beginning during the month.
- `people_affected`: sum of EM-DAT Total Affected.
- `economic_damage_000usd`: sum of EM-DAT adjusted economic damages.
- `total_deaths`: sum of EM-DAT Total Deaths.

Disaster counts are also constructed separately for:
- floods
- storms
- droughts
- wildfires
- extreme temperatures
- wet mass movements

## Severity measures

Population exposure:

`people_affected_pct_pop = 100 × people_affected / population`

Economic-damage exposure:

`damage_pct_gdp = 100 × (economic_damage_000usd × 1000) / gdp_current_usd`