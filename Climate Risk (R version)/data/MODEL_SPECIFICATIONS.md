# Model Specifications

The baseline panel specification is:

`log(RV_it) = β ClimateShock_it + γ'Controls_it + α_i + λ_t + ε_it`

where:

- `α_i` = country fixed effects
- `λ_t` = year-month fixed effects
- `Controls_it` includes VIX, WTI oil price, US 10-year Treasury yield, exchange-rate depreciation, GDP growth, inflation, and private credit/GDP.

The primary estimations use Driscoll-Kraay standard errors.

## Model 1

Climate-disaster dummy without controls:

`log(RV_it) = β DisasterDummy_it + α_i + λ_t + ε_it`

## Model 2 — Baseline

Climate-disaster dummy plus controls:

`log(RV_it) = β DisasterDummy_it + γ'Controls_it + α_i + λ_t + ε_it`

## Model 3

Number of climate disasters:

`log(RV_it) = β DisasterCount_it + γ'Controls_it + α_i + λ_t + ε_it`

## Model 4 — Severity

`log(RV_it) = β1 AffectedPopulationShare_it + β2 DamageGDPShare_it + γ'Controls_it + α_i + λ_t + ε_it`

## Model 5 — Disaster type

Includes separate monthly counts for floods, storms, droughts, wildfires, extreme temperatures, and wet mass movements.

## Model 6 — Alternative disaster definition

Uses a disaster dummy excluding wet mass movements.

## Standard-error robustness

The baseline model is additionally estimated with standard errors clustered by country.

## Country heterogeneity

The disaster dummy is interacted with country indicators, with India used as the reference category.

## Seasonality robustness

The baseline specification is additionally estimated with country × calendar-month fixed effects, alongside common year-month fixed effects, to absorb country-specific seasonal patterns.