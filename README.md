# production_planning_group75

Group 75's **Production Planning** application, built with R Shiny and published on Posit Connect Cloud.
Each course topic is added as a separate module (one file in `R/`) and appears as a tab in the app menu.
Methods and notation follow Nahmias, *Production and Operations Analysis*.

## Current status

| Assignment | Topic | Status |
|---|---|---|
| Assignment 1 | Learning Curve | Done |
| Assignment 2 | Forecasting (Chapter 2) | Done and tested against the book examples |

## Modules

| Menu | Method | File |
|---|---|---|
| Learning Curve | Learning curve | `R/ogrenme.R` |
| Forecasting → Stationary series | Moving Average | `R/moving_average.R` |
| | Weighted Moving Average | `R/weighted_moving_average.R` |
| | Simple Exponential Smoothing | `R/exponential_smoothing.R` |
| | Seasonal Factors (quick and dirty) | `R/seasonal_factors.R` |
| Forecasting → Trend series | Holt's Method | `R/holt.R` |
| Forecasting → Seasonal series | Seasonal Decomposition (moving averages) | `R/seasonal_decomposition.R` |
| | Winters's Method | `R/winters.R` |
| Forecasting → Box-Jenkins | AR, MA, ARMA, ARIMA | `R/box_jenkins.R` |
| (shared) | Data upload, error measures, regression, plots, reports | `R/forecast_helpers.R` |

## Features (Forecasting)

- **Data upload:** Excel (`.xlsx`, `.xls`) or CSV. For Excel files the sheet can be chosen, then the demand column. A **Remove file** button clears the file and the results.
- **Run button:** nothing is computed until **Run** is pressed. Changing a setting has no effect until Run is pressed again.
- **Parameters:** entered manually or found by search ("Find the best ..."), using the lowest MSE, MAD or MAPE (lowest AIC for Box-Jenkins). The parameter table shows which one was used.
- **Initial values** (exponential smoothing, Holt, Winters): regression on the first periods, or entered manually. Winters can also use the book's initialization procedure.
- **Results:** forecasts, plot, and MAD / MSE / MAPE. The period from which the error measures are computed can be chosen.
- **Box-Jenkins:** ACF / PACF plots (original, differenced and residual series), parameter estimates and forecasts with 95% intervals.
- **Export:** after Run, results can be downloaded as a **PDF report** (plots + tables) or **Excel tables** (one sheet per table).

## Project structure

```
production_planning_group75/
├── app.R           # main menu: every module is added here
├── manifest.json   # package list for Connect Cloud
├── README.md
└── R/              # one file per module + shared helpers
```

## Running locally

1. Download the repository (Code → Download ZIP) and unzip it.
2. In RStudio, install the packages once:
   ```r
   install.packages(c("shiny", "readxl", "writexl", "rsconnect"))
   ```
3. Open `app.R` and press **Run App**.

## Adding a new module

1. Write `R/<module_name>.R` with two functions: `<module_name>_ui(id)` and `<module_name>_server(id)`. Every input/output id in the UI goes inside `ns()`.
2. Add a `tabPanel(...)` line to the `ui` and a `<module_name>_server("...")` line to the `server` in `app.R`.
3. Test with **Run App** in RStudio.
4. If a new package was used, regenerate the manifest:
   ```r
   rsconnect::writeManifest("path/to/production_planning_group75")
   ```
5. Upload the changed files to GitHub and check the app on Connect Cloud.

## Packages

`shiny`, `readxl` (reading Excel), `writexl` (Excel export). Plots, ARIMA, ACF and PACF use base R (`graphics`, `stats`).
