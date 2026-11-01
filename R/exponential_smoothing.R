# =====================================================================
# Simple Exponential Smoothing           Nahmias, Section 2.7
#   F_t = alpha * D_{t-1} + (1 - alpha) * F_{t-1}
#   Multiple-step-ahead forecasts equal the one-step-ahead forecast.
#
# Starting the method (initial value):
#   - Manual:      the user enters F_1 (as in Example 2.3).
#   - Regression:  a line a + b t is fitted to the first k periods
#                  (baseline). The level at the end of the baseline,
#                  a + b k, is used as F_{k+1}. Smoothing starts after k.
#   - Average:     the average of the first k periods is used as F_{k+1}
#                  (book's suggestion, Problem 27).
# =====================================================================

# start = period of the first forecast (1 or k + 1), F0 = that forecast.
ses_forecast <- function(D, alpha, start, F0) {
  n <- length(D)
  F <- rep(NA_real_, n + 1)          # F[n + 1] = next-period forecast
  F[start] <- F0
  if (start <= n) for (t in (start + 1):(n + 1)) {
    F[t] <- alpha * D[t - 1] + (1 - alpha) * F[t - 1]
  }
  F
}

exponential_smoothing_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      radioButtons(ns("mode"), "Smoothing constant alpha",
                   choices = c("Enter alpha manually" = "manual",
                               "Find the best alpha" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "alpha (0 < alpha <= 1)", 0.1,
                     min = 0.01, max = 1, step = 0.01)),
      conditionalPanel("input.mode == 'auto'", ns = ns, criterion_ui(ns)),
      hr(),
      radioButtons(ns("init"), "Initial value",
                   choices = c("Regression on the first k periods" = "regression",
                               "Average of the first k periods" = "average",
                               "Enter F1 manually" = "manual")),
      conditionalPanel("input.init != 'manual'", ns = ns,
        numericInput(ns("k"), "k (baseline periods)", 4, min = 2, step = 1)),
      conditionalPanel("input.init == 'manual'", ns = ns,
        numericInput(ns("F1"), "F1 (forecast for period 1)", NA)),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        h4("Error measure for each alpha"),
        plotOutput(ns("search_plot"), height = "250px"))
    )
  )
}

exponential_smoothing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    init <- reactive({
      d <- D()
      if (P()$init == "manual") {
        F1 <- if (is.na(P()$F1)) d[1] else P()$F1
        list(start = 1, F0 = F1, a = NA, b = NA, k = 0)
      } else {
        k <- P()$k
        validate(need(k >= 2 && k < length(d), "k must be between 2 and n - 1."))
        if (P()$init == "regression") {
          fit <- regression_fit(d, k)
          list(start = k + 1, F0 = fit$a + fit$b * k, a = fit$a, b = fit$b, k = k)
        } else {
          list(start = k + 1, F0 = mean(d[1:k]), a = NA, b = NA, k = k)
        }
      }
    })

    search <- reactive({
      req(P()$mode == "auto")
      d <- D(); i <- init()
      alphas <- smoothing_grid(0.01)
      res <- t(sapply(alphas, function(a) {
        F <- ses_forecast(d, a, i$start, i$F0)[1:length(d)]
        c(alpha = a, forecast_errors(d, F, eval_start(P()$eval_from, F)))
      }))
      as.data.frame(res)
    })

    alpha <- reactive({
      if (P()$mode == "auto") {
        s <- search(); s$alpha[which.min(s[[P()$criterion]])]
      } else {
        validate(need(P()$alpha > 0 && P()$alpha <= 1, "alpha must be in (0, 1]."))
        P()$alpha
      }
    })

    result <- reactive({
      d <- D(); n <- length(d); i <- init()
      Fall <- ses_forecast(d, alpha(), i$start, i$F0)
      F <- Fall[1:n]
      future <- rep(Fall[n + 1], P()$horizon)
      from <- eval_start(P()$eval_from, F)
      list(F = F, future = future, err = forecast_errors(d, F, from), from = from)
    })

    output$forecast_plot <- renderPlot({
      r <- result()
      plot_forecast(D(), r$F, r$future,
                    paste0("Simple exponential smoothing, alpha = ", alpha()))
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable({
      i <- init()
      param_table(Selection = selection_text(P()), alpha = alpha(),
                  `Initial method` = P()$init,
                  `Regression intercept a` = if (is.na(i$a)) "-" else round(i$a, 4),
                  `Regression slope b` = if (is.na(i$b)) "-" else round(i$b, 4),
                  `First forecast period` = i$start,
                  `Initial forecast` = i$F0,
                  `Errors computed from period` = result()$from)
    })
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F, result()$future), na = "")
    output$search_plot <- renderPlot({
      s <- search()
      plot(s$alpha, s[[P()$criterion]], type = "l", lwd = 2,
           xlab = "alpha", ylab = P()$criterion)
      abline(v = alpha(), col = "firebrick", lty = 2)
    })

    report <- reactive({
      d <- D(); r <- result(); i <- init(); a <- alpha()
      plots <- list(function() plot_forecast(d, r$F, r$future,
                      paste0("Simple exponential smoothing, alpha = ", a)))
      tabs <- list(
        `Parameters` = param_table(Selection = selection_text(P()), alpha = a, `Initial method` = P()$init,
                                   `First forecast period` = i$start,
                                   `Initial forecast` = i$F0,
                                   `Errors computed from period` = r$from),
        `Error measures` = errors_table(r$err),
        `Forecasts` = forecast_table(d, r$F, r$future))
      if (P()$mode == "auto") {
        s <- search(); crit <- P()$criterion
        plots[[2]] <- function() {
          plot(s$alpha, s[[crit]], type = "l", lwd = 2, xlab = "alpha", ylab = crit,
               main = paste(crit, "for each alpha")); abline(v = a, col = "firebrick", lty = 2)
        }
      }
      list(title = "Simple Exponential Smoothing", plots = plots, tables = tabs)
    })
    register_downloads(output, report, "exponential_smoothing")
  })
}
