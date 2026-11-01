# =====================================================================
# Holt's Method (Double Exponential Smoothing)   Nahmias, Section 2.8
#   S_t = alpha * D_t + (1 - alpha) * (S_{t-1} + G_{t-1})
#   G_t = beta * (S_t - S_{t-1}) + (1 - beta) * G_{t-1}
#   F_{t, t+tau} = S_t + tau * G_t        (one step ahead: F_t = S_{t-1} + G_{t-1})
#
# Starting the method:
#   - Manual:     user enters S_0 and G_0 (time 0, as in Example 2.5).
#   - Regression: a line a + b t is fitted to the first k periods.
#                 S_k = a + b k, G_k = b. Smoothing starts after k.
# =====================================================================

# k0 = time of the initial values (0 or k), S0/G0 = initial values.
holt_run <- function(D, alpha, beta, k0, S0, G0) {
  n <- length(D)
  S <- rep(NA_real_, n); G <- rep(NA_real_, n); F <- rep(NA_real_, n)
  S_prev <- S0; G_prev <- G0
  if (k0 < n) for (t in (k0 + 1):n) {
    F[t] <- S_prev + G_prev
    S[t] <- alpha * D[t] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    S_prev <- S[t]; G_prev <- G[t]
  }
  list(S = S, G = G, F = F, S_last = S_prev, G_last = G_prev)
}

holt_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      radioButtons(ns("mode"), "Smoothing constants",
                   choices = c("Enter alpha and beta manually" = "manual",
                               "Find the best alpha and beta" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "alpha (level)", 0.1, min = 0.01, max = 1, step = 0.01),
        numericInput(ns("beta"), "beta (trend)", 0.1, min = 0.01, max = 1, step = 0.01)),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        criterion_ui(ns),
        helpText("Searches alpha and beta from 0.05 to 1 in steps of 0.05.")),
      hr(),
      radioButtons(ns("init"), "Initial level and slope",
                   choices = c("Regression on the first k periods" = "regression",
                               "Enter S0 and G0 manually" = "manual")),
      conditionalPanel("input.init == 'regression'", ns = ns,
        numericInput(ns("k"), "k (baseline periods)", 4, min = 2, step = 1)),
      conditionalPanel("input.init == 'manual'", ns = ns,
        numericInput(ns("S0"), "S0 (initial level / intercept)", 0),
        numericInput(ns("G0"), "G0 (initial slope)", 0)),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns),
      h4("Level and trend estimates"),
      tableOutput(ns("state_table"))
    )
  )
}

holt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    init <- reactive({
      d <- D()
      if (P()$init == "manual") {
        validate(need(!is.na(P()$S0) && !is.na(P()$G0), "Enter S0 and G0."))
        list(k0 = 0, S0 = P()$S0, G0 = P()$G0)
      } else {
        k <- P()$k
        validate(need(k >= 2 && k < length(d), "k must be between 2 and n - 1."))
        fit <- regression_fit(d, k)
        list(k0 = k, S0 = fit$a + fit$b * k, G0 = fit$b, a = fit$a)
      }
    })

    params <- reactive({
      if (P()$mode == "auto") {
        d <- D(); i <- init()
        grid <- expand.grid(alpha = smoothing_grid(0.05), beta = smoothing_grid(0.05))
        score <- apply(grid, 1, function(p) {
          F <- holt_run(d, p[1], p[2], i$k0, i$S0, i$G0)$F
          forecast_errors(d, F, eval_start(P()$eval_from, F))[[P()$criterion]]
        })
        best <- grid[which.min(score), ]
        c(alpha = best$alpha, beta = best$beta)
      } else {
        validate(need(P()$alpha > 0 && P()$alpha <= 1 &&
                      P()$beta > 0 && P()$beta <= 1,
                      "alpha and beta must be in (0, 1]."))
        c(alpha = P()$alpha, beta = P()$beta)
      }
    })

    result <- reactive({
      d <- D(); i <- init(); p <- params()
      h <- holt_run(d, p[["alpha"]], p[["beta"]], i$k0, i$S0, i$G0)
      future <- h$S_last + (1:P()$horizon) * h$G_last
      from <- eval_start(P()$eval_from, h$F)
      list(h = h, F = h$F, future = future,
           err = forecast_errors(d, h$F, from), from = from)
    })

    output$forecast_plot <- renderPlot({
      r <- result(); p <- params()
      plot_forecast(D(), r$F, r$future,
                    sprintf("Holt's method, alpha = %.2f, beta = %.2f",
                            p[["alpha"]], p[["beta"]]))
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable({
      i <- init(); p <- params()
      param_table(Selection = selection_text(P()), alpha = p[["alpha"]], beta = p[["beta"]],
                  `Initial method` = P()$init,
                  `Initial values at period` = i$k0,
                  `Initial level S` = i$S0, `Initial slope G` = i$G0,
                  `Errors computed from period` = result()$from)
    })
    output$state_table <- renderTable({
      h <- result()$h
      data.frame(Period = as.integer(seq_along(D())), Demand = D(),
                 S = round(h$S, 3), G = round(h$G, 3))
    }, na = "")
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F, result()$future), na = "")

    report <- reactive({
      d <- D(); r <- result(); i <- init(); p <- params()
      list(title = "Holt's Method",
           plots = list(function() plot_forecast(d, r$F, r$future,
             sprintf("Holt's method, alpha = %.2f, beta = %.2f", p[["alpha"]], p[["beta"]]))),
           tables = list(
             `Parameters` = param_table(Selection = selection_text(P()), alpha = p[["alpha"]], beta = p[["beta"]],
                                        `Initial method` = P()$init,
                                        `Initial values at period` = i$k0,
                                        `Initial level S` = i$S0, `Initial slope G` = i$G0,
                                        `Errors computed from period` = r$from),
             `Error measures` = errors_table(r$err),
             `Forecasts` = forecast_table(d, r$F, r$future),
             `Level and trend` = data.frame(Period = seq_along(d), Demand = d,
                                            S = round(r$h$S, 3), G = round(r$h$G, 3))))
    })
    register_downloads(output, report, "holt")
  })
}
