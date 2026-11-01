# =====================================================================
# Weighted Moving Average               Nahmias, Sections 2.5 and 2.6
#   F_t = w_1 D_{t-N} + ... + w_N D_{t-1}     (w_N = most recent weight)
#   Weights are scaled so that they add up to 1.
#   "Find the best weights" minimizes the chosen error measure
#   (weights are kept positive and adding to 1).
# =====================================================================

wma_forecast <- function(D, w) {
  n <- length(D)
  N <- length(w)
  F <- rep(NA_real_, n)
  if (N < n) for (t in (N + 1):n) F[t] <- sum(w * D[(t - N):(t - 1)])
  F
}

# Reads "0.1, 0.1, 0.2" into a numeric vector.
parse_weights <- function(txt) {
  w <- suppressWarnings(as.numeric(trimws(strsplit(txt, "[;,[:space:]]+")[[1]])))
  w[!is.na(w)]
}

# Searches the best weights for a given N.
wma_search <- function(D, N, criterion) {
  to_w <- function(z) exp(z) / sum(exp(z))
  obj <- function(z) {
    F <- wma_forecast(D, to_w(z))
    forecast_errors(D, F, from = N + 1)[[criterion]]
  }
  best <- optim(rep(0, N), obj, method = "Nelder-Mead",
                control = list(maxit = 5000, reltol = 1e-10))
  to_w(best$par)
}

weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      radioButtons(ns("mode"), "How should the weights be chosen?",
                   choices = c("Enter weights manually" = "manual",
                               "Find the best weights" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        textInput(ns("weights"),
                  "Weights, oldest to most recent (comma separated)",
                  "0.1, 0.1, 0.2, 0.2, 0.4")),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        numericInput(ns("N"), "Number of weights N", 3, min = 1, step = 1),
        criterion_ui(ns)),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns),
      h4("Weights used (oldest to most recent)"),
      tableOutput(ns("weight_table"))
    )
  )
}

weighted_moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    weights <- reactive({
      d <- D()
      if (P()$mode == "auto") {
        validate(need(P()$N >= 1 && P()$N < length(d) - 1,
                      "N must be smaller than the number of data points - 1."))
        wma_search(d, P()$N, P()$criterion)
      } else {
        w <- parse_weights(P()$weights)
        validate(need(length(w) >= 1 && all(w >= 0) && sum(w) > 0,
                      "Enter non-negative weights, for example: 0.2, 0.3, 0.5"),
                 need(length(w) < length(d), "Too many weights for this data."))
        w / sum(w)
      }
    })

    result <- reactive({
      d <- D()
      w <- weights()
      F <- wma_forecast(d, w)
      future <- rep(sum(w * tail(d, length(w))), P()$horizon)
      from <- eval_start(P()$eval_from, F)
      list(F = F, future = future, err = forecast_errors(d, F, from), from = from)
    })

    output$forecast_plot <- renderPlot({
      r <- result()
      plot_forecast(D(), r$F, r$future,
                    paste0("Weighted moving average (N = ", length(weights()), ")"))
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable(param_table(Selection = selection_text(P()), 
      N = length(weights()), `Errors computed from period` = result()$from))
    output$weight_table <- renderTable({
      w <- weights()
      data.frame(`Periods ago` = as.integer(length(w):1), Weight = round(w, 4),
                 check.names = FALSE)
    })
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F, result()$future), na = "")

    report <- reactive({
      d <- D(); r <- result(); w <- weights()
      list(title = "Weighted Moving Average",
           plots = list(function() plot_forecast(d, r$F, r$future,
             paste0("Weighted moving average (N = ", length(w), ")"))),
           tables = list(
             `Parameters` = param_table(Selection = selection_text(P()), N = length(w), `Errors computed from period` = r$from),
             `Weights` = data.frame(`Periods ago` = length(w):1, Weight = round(w, 4),
                                    check.names = FALSE),
             `Error measures` = errors_table(r$err),
             `Forecasts` = forecast_table(d, r$F, r$future)))
    })
    register_downloads(output, report, "weighted_moving_average")
  })
}
