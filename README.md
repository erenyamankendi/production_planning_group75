# EMU 341 Production Planning and Control - Group 75

An R Shiny course application with shared data management, a four-group
Forecasting workspace, and a learning curve module. Application text and code
comments are in English.

## Run locally

From the parent `production_planning` workspace in RStudio:

```r
source("setup.R")    # Install missing application packages into the local library.
source("run_app.R")  # Start the app at http://127.0.0.1:3838.
```

Alternatively, open this directory in RStudio and use **Run App** on `app.R`.
The launcher also accepts a `PLANNING_PORT` environment variable.

## Data and calculations

Upload XLSX or CSV data in **Data Management**, select the numeric target and
period column, then choose the calendar interval when using dates. Excel sheet
selection and explicit CSV separator/decimal controls are available. Row order
can be used when no period column exists. Edit cells with a double-click; new
target cells are intentionally empty. Sorting and deletion require explicit
user actions. Invalid, duplicate or irregular periods are reported rather
than silently changed.

Within **Forecasting**, choose Stationary Series, Trend Series, Seasonal Series
or Box-Jenkins, select a method, set its parameters, and click **Run Forecast**.
The horizon is one period. Changing data or settings invalidates old results.
Each method provides forecasts, a plot, MAD, MSE and MAPE. Box-Jenkins methods
also provide coefficient estimates, ACF/PACF, residual diagnostics and
model-based prediction intervals.

Errors are actual minus forecast. Metrics include only the displayed evaluation
periods; future periods and unavailable warm-up forecasts are excluded. MAPE is
unavailable when an evaluated actual is zero. Historical fitted errors and
parameter-selection scores are explicitly labelled and are not presented as
independent test accuracy. Automatic moving-average windows share a common
evaluation period; ties select the smaller window.

Smoothing supports manual or regression initialization. The initial state
refers to the end of the selected initialization prefix; regression never uses
later observations to determine that state. Winters estimates seasonal starts
from at least two cycles and supports additive or multiplicative seasonality.
Multiplicative seasonal methods require positive observations. Classical
decomposition uses centered moving averages followed by a linear trend on the
seasonally adjusted series; its historical fit uses the fitted sample.

Automatic ARIMA selects by AICc within the displayed bounds. Thorough search
disables stepwise selection; both search modes disable likelihood approximation.
Seasonal ARIMA search requires an explicit season length and its checkbox.
Model assumptions, optimizer failures and remaining residual correlation should
be reviewed; a small historical error alone does not establish forecast quality.

## Verification and structure

```r
source("production_planning_group75/test_models.R") # From the parent workspace.
```

Or run `Rscript --vanilla tests/run_tests.R` from this application directory.
The suite checks numerical reference cases, initialization, period validation,
CSV/XLSX input and module result invalidation.

- `app.R`: common application navigation and module wiring.
- `R/data_management.R`: shared dataset editing and validation.
- `R/forecast_core.R`: calculation functions independent of the interface.
- `R/forecasting.R`: namespaced forecasting interfaces and servers.
- `R/ogrenme.R`: existing learning curve module, with English interface text.
- `tests/fixtures/seasonal_demand.csv`: deterministic synthetic example data.

Before publishing, regenerate `manifest.json` with `rsconnect::writeManifest()`
from this application directory and verify the deployed app on Connect Cloud.
Exclude the parent workspace's local package library from version control.
