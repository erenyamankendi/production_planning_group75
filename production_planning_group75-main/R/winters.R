# =====================================================================
# Winters's Method (Triple Exponential Smoothing)   Nahmias, Section 2.9
#   S_t = alpha * (D_t / c_{t-N}) + (1 - alpha) * (S_{t-1} + G_{t-1})
#   G_t = beta * (S_t - S_{t-1}) + (1 - beta) * G_{t-1}
#   c_t = gamma * (D_t / S_t) + (1 - gamma) * c_{t-N}
#   F_{t, t+tau} = (S_t + tau * G_t) * c_{t+tau-N}
#   After each update the last N factors can be normed to add up to N.
#
# The first m seasons (baseline, m >= 2) are used to start the method.
# "Time 0" in the book is the end of the baseline.
#   - Winters' procedure (book): V_1..V_m season means,
#        G_0 = (V_m - V_1) / [(m - 1) N],  S_0 = V_m + G_0 (N - 1) / 2,
#        c_t = D_t / (V_i - [(N + 1)/2 - j] G_0), averaged and normed.
#   - Regression: line a + b t on the baseline, S_0 = a + b B, G_0 = b,
#        c_t = D_t / (a + b t), averaged and normed.
#   - Manual: user enters S_0 and G_0, factors from Winters' procedure.
# =====================================================================

winters_init_book <- function(D, N, m) {
  V <- sapply(1:m, function(i) mean(D[((i - 1) * N + 1):(i * N)]))
  G0 <- (V[m] - V[1]) / ((m - 1) * N)
  S0 <- V[m] + G0 * (N - 1) / 2
  ct <- matrix(NA, m, N)
  for (i in 1:m) for (j in 1:N) {
    ct[i, j] <- D[(i - 1) * N + j] / (V[i] - ((N + 1) / 2 - j) * G0)
  }
  cf <- colMeans(ct)
  list(S0 = S0, G0 = G0, factors = cf / sum(cf) * N, V = V)
}

winters_init_regression <- function(D, N, m) {
  B <- m * N
  fit <- regression_fit(D, B)
  line <- fit$a + fit$b * (1:B)
  ct <- matrix(D[1:B] / line, nrow = m, byrow = TRUE)
  cf <- colMeans(ct)
  list(S0 = fit$a + fit$b * B, G0 = fit$b, factors = cf / sum(cf) * N, a = fit$a)
}

# Runs Winters' method after the baseline B = m N.
winters_run <- function(D, N, B, S0, G0, c0, alpha, beta, gamma, norm = TRUE) {
  n <- length(D)
  S <- rep(NA_real_, n); G <- rep(NA_real_, n); F <- rep(NA_real_, n)
  C <- matrix(NA_real_, n, 1)
  cf <- c0                      # current factor for each season position
  S_prev <- S0; G_prev <- G0
  if (B < n) for (t in (B + 1):n) {
    p <- season_position(t, N)
    F[t] <- (S_prev + G_prev) * cf[p]
    S[t] <- alpha * (D[t] / cf[p]) + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    cf[p] <- gamma * (D[t] / S[t]) + (1 - gamma) * cf[p]
    if (norm) cf <- cf / sum(cf) * N
    C[t] <- cf[p]
    S_prev <- S[t]; G_prev <- G[t]
  }
  list(S = S, G = G, C = as.numeric(C), F = F,
       S_last = S_prev, G_last = G_prev, factors = cf)
}

winters_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      numericInput(ns("N"), "Season length N", 4, min = 2, step = 1),
      radioButtons(ns("mode"), "Smoothing constants",
                   choices = c("Enter alpha, beta, gamma manually" = "manual",
                               "Find the best values" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("alpha"), "alpha (series)", 0.2, min = 0.01, max = 1, step = 0.01),
        numericInput(ns("beta"), "beta (trend)", 0.1, min = 0.01, max = 1, step = 0.01),
        numericInput(ns("gamma"), "gamma (seasonal factors)", 0.1,
                     min = 0.01, max = 1, step = 0.01)),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        criterion_ui(ns),
        helpText("Searches each constant from 0.05 to 1 in steps of 0.05.")),
      hr(),
      numericInput(ns("m"), "Seasons used to start the method (baseline, m >= 2)",
                   2, min = 2, step = 1),
      radioButtons(ns("init"), "Initial level and slope",
                   choices = c("Winters' procedure (book)" = "book",
                               "Regression on the baseline" = "regression",
                               "Enter S0 and G0 manually" = "manual")),
      conditionalPanel("input.init == 'manual'", ns = ns,
        numericInput(ns("S0"), "S0 (level at the end of the baseline)", NA),
        numericInput(ns("G0"), "G0 (slope)", NA),
        helpText("Seasonal factors are computed with Winters' procedure.")),
      checkboxInput(ns("norm"), "Norm the factors to add up to N after each update", TRUE),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns, extra_top = tagList(
        h4("Seasonal factors (initial and final)"), tableOutput(ns("factor_table")))),
      h4("Series, trend and factor estimates"),
      tableOutput(ns("state_table"))
    )
  )
}

winters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    init <- reactive({
      d <- D(); N <- P()$N; m <- P()$m
      validate(need(N >= 2 && m >= 2, "N and m must be at least 2."),
               need(length(d) > m * N,
                    "Need more data than the baseline (m seasons x N periods)."))
      if (P()$init == "regression") return(winters_init_regression(d, N, m))
      b <- winters_init_book(d, N, m)
      if (P()$init == "manual") {
        validate(need(!is.na(P()$S0) && !is.na(P()$G0), "Enter S0 and G0."))
        b$S0 <- P()$S0; b$G0 <- P()$G0
      }
      b
    })

    params <- reactive({
      if (P()$mode == "auto") {
        d <- D(); i <- init(); N <- P()$N; B <- P()$m * N
        g <- smoothing_grid(0.05)
        grid <- expand.grid(alpha = g, beta = g, gamma = g)
        score <- apply(grid, 1, function(p) {
          F <- winters_run(d, N, B, i$S0, i$G0, i$factors, p[1], p[2], p[3],
                           P()$norm)$F
          forecast_errors(d, F, eval_start(P()$eval_from, F))[[P()$criterion]]
        })
        unlist(grid[which.min(score), ])
      } else {
        v <- c(alpha = P()$alpha, beta = P()$beta, gamma = P()$gamma)
        validate(need(all(v > 0 & v <= 1), "Smoothing constants must be in (0, 1]."))
        v
      }
    })

    result <- reactive({
      d <- D(); n <- length(d); N <- P()$N; i <- init(); p <- params()
      w <- winters_run(d, N, P()$m * N, i$S0, i$G0, i$factors,
                       p[["alpha"]], p[["beta"]], p[["gamma"]], P()$norm)
      tau <- 1:P()$horizon
      future <- (w$S_last + tau * w$G_last) * w$factors[season_position(n + tau, N)]
      from <- eval_start(P()$eval_from, w$F)
      list(w = w, F = w$F, future = future,
           err = forecast_errors(d, w$F, from), from = from)
    })

    output$factor_table <- renderTable({
      data.frame(`Season period` = as.integer(1:P()$N),
                 Initial = round(init()$factors, 4),
                 Final = round(result()$w$factors, 4), check.names = FALSE)
    })
    output$state_table <- renderTable({
      w <- result()$w
      data.frame(Period = as.integer(seq_along(D())), Demand = D(),
                 S = round(w$S, 3), G = round(w$G, 4), `Updated c` = round(w$C, 4),
                 check.names = FALSE)
    }, na = "")
    output$forecast_plot <- renderPlot({
      r <- result(); p <- params()
      plot_forecast(D(), r$F, r$future,
                    sprintf("Winters's method, alpha = %.2f, beta = %.2f, gamma = %.2f",
                            p[["alpha"]], p[["beta"]], p[["gamma"]]))
      abline(v = P()$m * P()$N + 0.5, col = "grey40", lty = 2)
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable({
      i <- init(); p <- params()
      param_table(Selection = selection_text(P()), alpha = p[["alpha"]], beta = p[["beta"]], gamma = p[["gamma"]],
                  N = P()$N, `Baseline periods` = P()$m * P()$N,
                  `Initial method` = P()$init,
                  `S0 (end of baseline)` = i$S0, G0 = i$G0,
                  `Errors computed from period` = result()$from)
    })
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F, result()$future), na = "")

    report <- reactive({
      d <- D(); r <- result(); i <- init(); p <- params(); w <- r$w
      base_end <- P()$m * P()$N
      list(title = "Winters's Method",
           plots = list(function() {
             plot_forecast(d, r$F, r$future,
               sprintf("Winters's method, alpha = %.2f, beta = %.2f, gamma = %.2f",
                       p[["alpha"]], p[["beta"]], p[["gamma"]]))
             abline(v = base_end + 0.5, col = "grey40", lty = 2)
           }),
           tables = list(
             `Parameters` = param_table(Selection = selection_text(P()), alpha = p[["alpha"]], beta = p[["beta"]],
                                        gamma = p[["gamma"]], N = P()$N,
                                        `Baseline periods` = base_end,
                                        `Initial method` = P()$init,
                                        `S0 (end of baseline)` = i$S0, G0 = i$G0,
                                        `Errors computed from period` = r$from),
             `Seasonal factors` = data.frame(`Season period` = 1:P()$N,
                                             Initial = round(i$factors, 4),
                                             Final = round(w$factors, 4),
                                             check.names = FALSE),
             `Error measures` = errors_table(r$err),
             `Forecasts` = forecast_table(d, r$F, r$future),
             `Estimates` = data.frame(Period = seq_along(d), Demand = d,
                                      S = round(w$S, 3), G = round(w$G, 4),
                                      `Updated c` = round(w$C, 4), check.names = FALSE)))
    })
    register_downloads(output, report, "winters")
  })
}
