# =====================================================================
# Moving Average - MA(N)            Nahmias, Section 2.7
#   F_t = (D_{t-1} + D_{t-2} + ... + D_{t-N}) / N
#   Multiple-step-ahead forecasts equal the one-step-ahead forecast.
# =====================================================================

ma_forecast <- function(D, N) {
  n <- length(D)
  F <- rep(NA_real_, n)
  if (N < n) for (t in (N + 1):n) F[t] <- mean(D[(t - N):(t - 1)])
  F
}

moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      radioButtons(ns("mode"), "How should N be chosen?",
                   choices = c("Enter N manually" = "manual",
                               "Find the best N" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        numericInput(ns("N"), "N (number of periods averaged)", 3, min = 1, step = 1)),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        numericInput(ns("Nmax"), "Largest N to try", 12, min = 2, step = 1),
        criterion_ui(ns)),
      common_settings_ui(ns)
    ),
    mainPanel(
      results_ui(ns),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        h4("Search results (all N compared over the same periods)"),
        tableOutput(ns("search_table")))
    )
  )
}

moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P

    search <- reactive({
      req(P()$mode == "auto")
      d <- D()
      Nmax <- min(P()$Nmax, length(d) - 1)
      validate(need(Nmax >= 1, "Not enough data for the search."))
      res <- lapply(1:Nmax, function(N) {
        c(N = N, forecast_errors(d, ma_forecast(d, N), from = Nmax + 1))
      })
      as.data.frame(do.call(rbind, res))
    })

    N <- reactive({
      if (P()$mode == "auto") {
        s <- search()
        s$N[which.min(s[[P()$criterion]])]
      } else {
        validate(need(P()$N >= 1 && P()$N < length(D()),
                      "N must be at least 1 and smaller than the number of data points."))
        P()$N
      }
    })

    result <- reactive({
      d <- D()
      F <- ma_forecast(d, N())
      future <- rep(mean(tail(d, N())), P()$horizon)
      from <- eval_start(P()$eval_from, F)
      list(F = F, future = future, err = forecast_errors(d, F, from), from = from)
    })

    output$forecast_plot <- renderPlot({
      r <- result()
      plot_forecast(D(), r$F, r$future, paste0("Moving average, MA(", N(), ")"))
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable(param_table(Selection = selection_text(P()), 
      N = N(), `Errors computed from period` = result()$from))
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F, result()$future), na = "")
    output$search_table <- renderTable(search(), digits = 4)

    report <- reactive({
      d <- D(); r <- result(); n_used <- N()
      tabs <- list(
        `Parameters` = param_table(Selection = selection_text(P()), N = n_used, `Errors computed from period` = r$from),
        `Error measures` = errors_table(r$err),
        `Forecasts` = forecast_table(d, r$F, r$future))
      if (P()$mode == "auto") tabs$`N search` <- search()
      list(title = "Moving Average",
           plots = list(function() plot_forecast(d, r$F, r$future,
                                                 paste0("Moving average, MA(", n_used, ")"))),
           tables = tabs)
    })
    register_downloads(output, report, "moving_average")
  })
}
