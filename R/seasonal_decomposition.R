# =====================================================================
# Seasonal Decomposition Using Moving Averages    Nahmias, Section 2.9
#   1. Centered moving average (CMA) of length N.
#      N odd : average of the N periods around t.
#      N even: average of two neighbouring N-period averages.
#   2. Ratio D_t / CMA_t for every period that has a CMA.
#   3. Average the ratios of like periods, then norm so the factors add to N.
#   4. Deseasonalize: D_t / c_t.
#   5. Forecast the deseasonalized series (regression line, Section 2.8,
#      or a moving average of the last M deseasonalized values).
#   6. Re-seasonalize: forecast = deseasonalized forecast * c_t.
# =====================================================================

centered_ma <- function(D, N) {
  n <- length(D)
  cma <- rep(NA_real_, n)
  if (N %% 2 == 1) {
    h <- (N - 1) / 2
    for (t in (h + 1):(n - h)) cma[t] <- mean(D[(t - h):(t + h)])
  } else {
    h <- N / 2
    if (n > N) for (t in (h + 1):(n - h)) {
      cma[t] <- (mean(D[(t - h):(t + h - 1)]) + mean(D[(t - h + 1):(t + h)])) / 2
    }
  }
  cma
}

decompose_ma <- function(D, N) {
  n <- length(D)
  cma <- centered_ma(D, N)
  ratio <- D / cma
  pos <- season_position(1:n, N)
  raw <- as.numeric(tapply(ratio, factor(pos, levels = 1:N), mean, na.rm = TRUE))
  cf <- raw / sum(raw) * N
  list(cma = cma, ratio = ratio, raw = raw, factors = cf,
       deseason = D / cf[pos])
}

seasonal_decomposition_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      numericInput(ns("N"), "Season length N", 4, min = 2, step = 1),
      radioButtons(ns("trend"), "Forecast the deseasonalized series with",
                   choices = c("Linear regression (trend)" = "regression",
                               "Moving average of the last M values" = "ma")),
      conditionalPanel("input.trend == 'ma'", ns = ns,
        radioButtons(ns("mode"), "M",
                     choices = c("Enter M manually" = "manual",
                                 "Find the best M" = "auto")),
        conditionalPanel("input.mode == 'manual'", ns = ns,
          numericInput(ns("M"), "M (periods averaged)", 6, min = 1, step = 1)),
        conditionalPanel("input.mode == 'auto'", ns = ns,
          numericInput(ns("Mmax"), "Largest M to try", 8, min = 2, step = 1),
          criterion_ui(ns))),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns, extra_top = tagList(
        h4("Seasonal factors"), tableOutput(ns("factor_table")))),
      h4("Decomposition steps"),
      tableOutput(ns("step_table"))
    )
  )
}

seasonal_decomposition_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    dec <- reactive({
      d <- D(); N <- P()$N
      validate(need(N >= 2 && length(d) >= 2 * N,
                    "Need at least two full seasons of data."))
      decompose_ma(d, N)
    })

    model <- reactive({
      d <- D(); n <- length(d); N <- P()$N; x <- dec()
      pos_all <- season_position(1:(n + P()$horizon), N)
      cf <- x$factors[pos_all]
      if (P()$trend == "regression") {
        fit <- regression_fit(x$deseason)
        base <- fit$a + fit$b * (1:(n + P()$horizon))
        F <- base[1:n] * cf[1:n]
        future <- base[(n + 1):(n + P()$horizon)] * cf[(n + 1):(n + P()$horizon)]
        list(F = F, future = future, a = fit$a, b = fit$b, M = NA,
             label = "Fitted value (regression x factor)")
      } else {
        des <- x$deseason
        run <- function(M) {
          Fd <- ma_forecast(des, M)
          Fd * cf[1:n]
        }
        if (P()$mode == "auto") {
          Mmax <- min(P()$Mmax, n - 1)
          sc <- sapply(1:Mmax, function(M)
            forecast_errors(d, run(M), from = Mmax + 1)[[P()$criterion]])
          M <- which.min(sc)
        } else {
          M <- P()$M
          validate(need(M >= 1 && M < n, "M must be between 1 and n - 1."))
        }
        level <- mean(tail(des, M))
        list(F = run(M), future = level * cf[(n + 1):(n + P()$horizon)],
             a = NA, b = NA, M = M, label = "One-step-ahead forecast")
      }
    })

    result <- reactive({
      m <- model()
      from <- eval_start(P()$eval_from, m$F)
      list(err = forecast_errors(D(), m$F, from), from = from)
    })

    output$factor_table <- renderTable({
      x <- dec()
      data.frame(`Season period` = as.integer(seq_along(x$factors)),
                 `Average ratio` = round(x$raw, 4),
                 `Normed factor` = round(x$factors, 4), check.names = FALSE)
    })
    output$step_table <- renderTable({
      x <- dec()
      data.frame(Period = as.integer(seq_along(D())), Demand = D(),
                 `Centered MA` = round(x$cma, 3), `D / CMA` = round(x$ratio, 4),
                 Deseasonalized = round(x$deseason, 3), check.names = FALSE)
    }, na = "")
    output$forecast_plot <- renderPlot({
      m <- model()
      plot_forecast(D(), m$F, m$future, "Seasonal decomposition (moving averages)",
                    fitted_label = m$label)
      lines(seq_along(D()), dec()$deseason, col = "darkgreen", lty = 3, lwd = 2)
      legend("bottomright", bty = "n", legend = "Deseasonalized series",
             col = "darkgreen", lty = 3, lwd = 2)
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable({
      m <- model()
      param_table(Selection = selection_text(P()), N = P()$N,
                  `Deseasonalized forecast` = P()$trend,
                  `Regression intercept a` = if (is.na(m$a)) "-" else round(m$a, 4),
                  `Regression slope b` = if (is.na(m$b)) "-" else round(m$b, 4),
                  M = if (is.na(m$M)) "-" else m$M,
                  `Errors computed from period` = result()$from)
    })
    output$forecast_table <- renderTable(
      forecast_table(D(), model()$F, model()$future), na = "")

    report <- reactive({
      d <- D(); m <- model(); x <- dec(); r <- result()
      list(title = "Seasonal Decomposition (moving averages)",
           plots = list(function() {
             plot_forecast(d, m$F, m$future, "Seasonal decomposition (moving averages)",
                           fitted_label = m$label)
             lines(seq_along(d), x$deseason, col = "darkgreen", lty = 3, lwd = 2)
           }),
           tables = list(
             `Parameters` = param_table(Selection = selection_text(P()), N = P()$N, `Deseasonalized forecast` = P()$trend,
                                        `Regression intercept a` = if (is.na(m$a)) "-" else round(m$a, 4),
                                        `Regression slope b` = if (is.na(m$b)) "-" else round(m$b, 4),
                                        M = if (is.na(m$M)) "-" else m$M,
                                        `Errors computed from period` = r$from),
             `Seasonal factors` = data.frame(`Season period` = seq_along(x$factors),
                                             `Average ratio` = round(x$raw, 4),
                                             `Normed factor` = round(x$factors, 4),
                                             check.names = FALSE),
             `Error measures` = errors_table(r$err),
             `Forecasts` = forecast_table(d, m$F, m$future),
             `Decomposition steps` = data.frame(Period = seq_along(d), Demand = d,
                                                `Centered MA` = round(x$cma, 3),
                                                `D over CMA` = round(x$ratio, 4),
                                                Deseasonalized = round(x$deseason, 3),
                                                check.names = FALSE)))
    })
    register_downloads(output, report, "seasonal_decomposition")
  })
}
