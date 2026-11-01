# =====================================================================
# Seasonal Factors for Stationary Series ("quick and dirty" method)
# Nahmias, Section 2.9, Example 2.6
#   1. Compute the sample mean of all the data.
#   2. Divide each observation by the sample mean.
#   3. Average the factors for like periods within each season.
#      The N factors add up to N.
#   4. Forecast = mean * seasonal factor of that period.
# Values for past periods are "fitted values" (mean * factor).
# =====================================================================

season_position <- function(t, N) ((t - 1) %% N) + 1

seasonal_factors_simple <- function(D, N) {
  m <- mean(D)
  ratio <- D / m
  pos <- season_position(seq_along(D), N)
  cf <- tapply(ratio, factor(pos, levels = 1:N), mean)
  cf <- as.numeric(cf)
  cf <- cf / sum(cf) * N   # exact sum N (matters only for incomplete seasons)
  list(mean = m, factors = cf)
}

seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      numericInput(ns("N"), "Season length N (periods in one season)", 4,
                   min = 2, step = 1),
      helpText("The method assumes no trend and needs at least two seasons of data."),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns, extra_top = tagList(
        h4("Seasonal factors"), tableOutput(ns("factor_table"))))
    )
  )
}

seasonal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    result <- reactive({
      d <- D(); n <- length(d); N <- P()$N
      validate(need(N >= 2 && n >= 2 * N, "Need at least two full seasons of data."))
      s <- seasonal_factors_simple(d, N)
      F <- s$mean * s$factors[season_position(1:n, N)]
      future <- s$mean * s$factors[season_position((n + 1):(n + P()$horizon), N)]
      from <- eval_start(P()$eval_from, F)
      list(s = s, F = F, future = future, err = forecast_errors(d, F, from), from = from)
    })

    output$factor_table <- renderTable({
      s <- result()$s
      data.frame(`Season period` = as.integer(seq_along(s$factors)),
                 Factor = round(s$factors, 4),
                 `Forecast (mean x factor)` = round(s$mean * s$factors, 3),
                 check.names = FALSE)
    })
    output$forecast_plot <- renderPlot({
      r <- result()
      plot_forecast(D(), r$F, r$future, "Seasonal factors (stationary series)",
                    fitted_label = "Fitted value (mean x factor)")
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable(param_table(
      N = P()$N, `Overall mean` = result()$s$mean,
      `Sum of factors` = sum(result()$s$factors),
      `Errors computed from period` = result()$from))
    output$forecast_table <- renderTable({
      tab <- forecast_table(D(), result()$F, result()$future)
      names(tab)[3] <- "Fitted / Forecast"
      tab
    }, na = "")

    report <- reactive({
      d <- D(); r <- result(); s <- r$s
      tab <- forecast_table(d, r$F, r$future); names(tab)[3] <- "Fitted / Forecast"
      list(title = "Seasonal Factors (quick and dirty)",
           plots = list(function() plot_forecast(d, r$F, r$future,
             "Seasonal factors (stationary series)",
             fitted_label = "Fitted value (mean x factor)")),
           tables = list(
             `Parameters` = param_table(N = P()$N, `Overall mean` = s$mean,
                                        `Errors computed from period` = r$from),
             `Seasonal factors` = data.frame(`Season period` = seq_along(s$factors),
                                             Factor = round(s$factors, 4),
                                             check.names = FALSE),
             `Error measures` = errors_table(r$err),
             `Forecasts` = tab))
    })
    register_downloads(output, report, "seasonal_factors")
  })
}
