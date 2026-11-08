pp_presentation_styles <- function() {
  shiny::tags$head(shiny::tags$style(shiny::HTML(
    ".pp-hint { background: #f3f7fa; border-left: 3px solid #46758c;
      color: #24384b; font-size: 16px; line-height: 1.45;
      padding: 9px 12px; margin: 0 0 14px; }
     .pp-hint strong { margin-right: 0.45em; }"
  )))
}

pp_presentation_hint <- function(label, detail) {
  shiny::tags$p(class = "pp-hint", shiny::tags$strong(label), detail)
}

pp_forecast_hint <- function(method) {
  detail <- switch(method,
    moving_average = "The next forecast is the average of the last N observations. A larger N smooths short-term changes.",
    weighted_average = "Choose how much each recent observation counts. Enter weights from newest to oldest so they sum to 1.",
    ses = "Tracks the series level. Higher alpha gives the most recent observation more influence.",
    holt = "Tracks both level and trend. Alpha updates the level and beta updates the slope.",
    seasonal_factors = "Estimates a repeating seasonal pattern around a stable level. A factor of 1.2 means 20% above the baseline.",
    decomposition = "Centered moving averages help isolate the seasonal pattern. The method forecasts the adjusted trend and restores seasonality.",
    winters = "Updates level, trend and seasonality together. Set the cycle length first. Automatic fitting needs observations after the initial cycles.",
    ar = "Uses past values of the series. The order p gives the number of past values used.",
    ma = "Uses past forecast shocks. The order q gives the number of past shocks used.",
    arma = "Combines past values and past shocks. Use it for a series with a stable level after any needed transformation.",
    arima = "Differencing removes persistent changes in level when appropriate. The orders p, d and q count past values, differences and past shocks.",
    auto_arima = "Searches candidate models using AICc. Open the diagnostics to check whether the residuals still show a pattern.")
  if (!is.null(detail)) pp_presentation_hint("Key idea", detail)
}

pp_plan_hint <- function(method) {
  detail <- switch(method,
    chase = "Adjust the workforce as demand changes. Carry stock when needed to cover closed periods or the ending stock target.",
    level = "Keep the same workforce throughout the horizon. Inventory built in quieter periods can cover later demand.",
    mixed = "Choose when the workforce can change. The plan keeps the preceding workforce in all other periods.",
    lp = "Minimize total cost while meeting demand and stock requirements. Enable overtime, idle time or subcontracting to test added flexibility.",
    transport_northwest = "Allocate supply in period order, starting at the upper-left eligible cell. This is a starting plan for comparison.",
    transport_least_cost = "Allocate to the cheapest eligible cell first. Compare its cost with the verified Minimum Cost plan.",
    transport_optimal = "Find the least costly feasible allocation across regular production, overtime and subcontracting. Earlier production can be carried forward as stock.",
    disaggregation = "Split in-house production among families. The square-root rule uses each family's setup cost and annual usage and respects entered bounds.")
  if (!is.null(detail)) pp_presentation_hint("Key idea", detail)
}
