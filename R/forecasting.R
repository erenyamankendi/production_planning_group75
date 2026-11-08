forecasting_method_ui <- function(id, method) {
  ns <- shiny::NS(id)
  catalog <- pp_catalog()
  label <- catalog$label[match(method, catalog$id)]
  smoothing <- method %in% c("ses", "holt", "winters")
  box <- method %in% c("ar", "ma", "arma", "arima", "auto_arima")
  seasonal <- method %in% c("seasonal_factors", "decomposition", "winters", "auto_arima")
  controls <- shiny::tagList()
  if (method %in% c("moving_average", "weighted_average")) {
    if (method == "moving_average") controls <- shiny::tagAppendChildren(controls,
      shiny::radioButtons(ns("parameter_mode"), "Window selection", c("Manual" = "manual", "Minimum MSE" = "auto")),
      shiny::conditionalPanel(sprintf("input['%s'] == 'manual'", ns("parameter_mode")),
        shiny::numericInput(ns("n"), "N: past periods", 3, min = 1, step = 1)),
      shiny::conditionalPanel(sprintf("input['%s'] == 'auto'", ns("parameter_mode")),
        shiny::numericInput(ns("search_max"), "Search upper limit (blank = half of observations)", NA, min = 1, step = 1),
        shiny::p(class = "help-block", "All candidate windows are scored on the same periods. Equal MSE selects the smaller N.")))
    else controls <- shiny::tagAppendChildren(controls,
      shiny::numericInput(ns("n"), "N: past periods", 3, min = 1, step = 1),
      shiny::textInput(ns("weights"), "Weights, most recent period first", "0.5, 0.3, 0.2"),
      shiny::p(class = "help-block", "Enter N non-negative weights separated by commas. Their sum must be 1."))
  }
  if (seasonal) controls <- shiny::tagAppendChildren(controls,
    shiny::numericInput(ns("period"), "Season length", if (method == "auto_arima") 1 else 4,
                        min = if (method == "auto_arima") 1 else 2, step = 1),
    shiny::numericInput(ns("start_phase"), "Season of the first observation", 1, min = 1, step = 1),
    shiny::p(class = "help-block", "Examples: 4 quarters per cycle, 12 months per cycle. Starting season identifies the first row's place in that cycle."))
  if (method %in% c("decomposition", "winters")) controls <- shiny::tagAppendChildren(controls,
    shiny::selectInput(ns("season_type"), "Seasonality", c("Additive" = "additive", "Multiplicative (positive data)" = "multiplicative")))
  if (smoothing) {
    controls <- shiny::tagAppendChildren(controls,
      shiny::radioButtons(ns("parameter_mode"), "Smoothing parameters", c("Manual" = "manual", "Optimize squared errors" = "auto")),
      shiny::conditionalPanel(sprintf("input['%s'] == 'manual'", ns("parameter_mode")),
        shiny::sliderInput(ns("alpha"), "Alpha: level", 0, 1, 0.2, step = 0.01),
        if (method != "ses") shiny::sliderInput(ns("beta"), "Beta: trend", 0, 1, 0.1, step = 0.01),
        if (method == "winters") shiny::sliderInput(ns("gamma"), "Gamma: seasonality", 0, 1, 0.1, step = 0.01)),
      shiny::hr(),
      shiny::radioButtons(ns("init_mode"), "Initialization", c("Regression" = "regression", "Manual" = "manual")),
      shiny::numericInput(ns("init_count"), "Initialization observations (blank = default)", NA, min = 1, step = 1),
      shiny::p(class = "help-block", if (method == "winters")
        "Default: first two cycles. Seasonal components are estimated from this prefix; regression uses its seasonally adjusted data."
        else "Default: first 3 observations for regression, or 1 for manual initialization. Initial states refer to the end of this prefix."),
      shiny::conditionalPanel(sprintf("input['%s'] == 'manual'", ns("init_mode")),
        shiny::numericInput(ns("init_level"), "Level at the end of initialization", NA),
        if (method != "ses") shiny::numericInput(ns("init_slope"), "Slope per period at the end of initialization", NA)))
  }
  if (box) {
    if (method == "auto_arima") controls <- shiny::tagAppendChildren(controls,
      shiny::numericInput(ns("max_p"), "Maximum p", 5, min = 0, max = 10, step = 1),
      shiny::numericInput(ns("max_q"), "Maximum q", 5, min = 0, max = 10, step = 1),
      shiny::checkboxInput(ns("seasonal_search"), "Consider seasonal ARIMA (season length > 1)", FALSE),
      shiny::selectInput(ns("search_mode"), "AICc search", c("Thorough (slower)" = "thorough", "Stepwise (faster)" = "fast")),
      shiny::p(class = "help-block", "Total AR/MA order is limited to 5. Differencing is selected separately. Approximate likelihood search is disabled."))
    else controls <- shiny::tagAppendChildren(controls,
      if (method != "ma") shiny::numericInput(ns("p"), "p: autoregressive order", 1,
                                                min = if (method == "arima") 0 else 1, max = 20, step = 1),
      if (method == "arima") shiny::numericInput(ns("d"), "d: differencing order", 1, min = 0, max = 2, step = 1),
      if (method != "ar") shiny::numericInput(ns("q"), "q: moving-average error order", 1,
                                                min = if (method == "arima") 0 else 1, max = 20, step = 1))
    controls <- shiny::tagAppendChildren(controls,
      shiny::checkboxInput(ns("include_constant"), "Allow a mean / drift where appropriate", TRUE),
      if (method == "ma") shiny::p(class = "help-block", "This MA model uses past shocks. It is different from the Moving Average forecasting method."))
  }
  shiny::sidebarLayout(
    shiny::sidebarPanel(shiny::h4("Model settings"), shiny::p("Data are supplied by Data Management."),
      controls, shiny::hr(), shiny::actionButton(ns("run"), "Run Forecast", class = "btn-primary", icon = shiny::icon("play"))),
    shiny::mainPanel(shiny::h3(label), pp_forecast_hint(method), shiny::uiOutput(ns("status")),
      shiny::h4(shiny::textOutput(ns("next_forecast"))),
      shiny::tabsetPanel(
        shiny::tabPanel("Forecast", shiny::plotOutput(ns("forecast_plot"), height = "340px"),
          shiny::h4("MAD, MSE and MAPE"),
          pp_presentation_hint("Read the errors", "MAD is in the original units. MSE gives more weight to large misses. MAPE expresses the error as a percentage."),
          shiny::textOutput(ns("accuracy_scope")),
          shiny::tableOutput(ns("metrics")), shiny::h4("Forecasts by period"), DT::DTOutput(ns("forecast_table"))),
        shiny::tabPanel("Method details", shiny::tableOutput(ns("parameters")),
          shiny::uiOutput(ns("extra_details")),
          if (method == "moving_average") shiny::tableOutput(ns("search_table")),
          if (method %in% c("seasonal_factors", "decomposition")) shiny::tableOutput(ns("seasonal_table")),
          if (method %in% c("seasonal_factors", "decomposition")) shiny::plotOutput(ns("component_plot")),
          if (smoothing || method == "decomposition") shiny::plotOutput(ns("regression_plot"))),
        if (box) shiny::tabPanel("Box-Jenkins diagnostics",
          pp_presentation_hint("Check the residuals", "ACF and PACF show how values are related across lags. Look for a pattern that the fitted model has left behind."),
          shiny::h4("Parameter estimates"), shiny::tableOutput(ns("estimates")),
          shiny::h4("Next-period prediction intervals"), shiny::tableOutput(ns("intervals")),
          shiny::plotOutput(ns("acf_original")), shiny::plotOutput(ns("acf_transformed")),
          shiny::plotOutput(ns("acf_residuals")), shiny::textOutput(ns("ljung_box")))
      )))
}

forecasting_method_server <- function(id, method, data_bundle) {
  shiny::moduleServer(id, function(input, output, session) {
    parameters <- shiny::reactive({
      params <- list(n = input$n, search_max = input$search_max,
        parameter_mode = input$parameter_mode, alpha = input$alpha, beta = input$beta, gamma = input$gamma,
        init_mode = input$init_mode, init_count = input$init_count,
        init_level = input$init_level, init_slope = input$init_slope,
        period = input$period, start_phase = input$start_phase, type = input$season_type,
        p = input$p, d = input$d, q = input$q, include_constant = input$include_constant,
        seasonal_search = input$seasonal_search, max_p = input$max_p, max_q = input$max_q,
        search_mode = input$search_mode)
      if (method == "weighted_average") {
        pieces <- trimws(strsplit(if (is.null(input$weights)) "" else input$weights, ",", fixed = TRUE)[[1]])
        params$weights <- suppressWarnings(as.numeric(pieces))
      }
      params
    })
    current <- shiny::reactive({
      data <- tryCatch(data_bundle(), error = identity)
      if (inherits(data, "error")) return(list(error = conditionMessage(data)))
      list(data = data, parameters = parameters())
    })
    outcome <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run, {
      signature <- current()
      if (!is.null(signature$error)) {
        outcome(list(signature = signature, error = signature$error)); return()
      }
      fit <- tryCatch(shiny::withProgress(message = "Calculating forecast", value = 0.3, {
        result <- pp_fit(method, signature$data$y, signature$parameters)
        shiny::incProgress(0.7)
        result
      }), error = identity)
      if (inherits(fit, "error")) outcome(list(signature = signature, error = conditionMessage(fit)))
      else outcome(list(signature = signature, result = fit, error = NULL))
    }, ignoreInit = TRUE)
    ready <- shiny::reactive({
      state <- outcome()
      if (is.null(state) || !is.null(state$error) || !identical(state$signature, current())) return(NULL)
      state$result
    })
    output$status <- shiny::renderUI({
      signature <- current()
      if (!is.null(signature$error)) return(shiny::div(class = "alert alert-info", signature$error))
      state <- outcome()
      if (is.null(state)) return(shiny::p("Select the model settings and click Run Forecast. The forecast horizon is one period."))
      if (!identical(state$signature, signature)) return(shiny::div(class = "alert alert-warning", "Data or settings changed. Click Run Forecast to calculate current results."))
      if (!is.null(state$error)) return(shiny::div(class = "alert alert-danger", state$error))
      result <- state$result
      shiny::tagList(shiny::p(sprintf("Series: %s | Observations: %d | Forecast horizon: 1 period", signature$data$name, length(result$actual))),
        if (length(result$warnings)) shiny::tags$details(shiny::tags$summary("Method notes"),
          shiny::tags$ul(lapply(result$warnings, shiny::tags$li))))
    })
    output$next_forecast <- shiny::renderText({
      result <- ready(); shiny::req(result)
      sprintf("Next-period forecast: %s", format(result$forecast, digits = 7, trim = TRUE))
    })
    output$accuracy_scope <- shiny::renderText({
      result <- ready(); shiny::req(result)
      metrics <- result$metrics
      periods <- current()$data$labels
      range <- if (metrics$count) sprintf("%s to %s", periods[min(metrics$periods)], periods[max(metrics$periods)]) else "none"
      sprintf("%s. Evaluated periods: %s (%d observations).", result$accuracy_label, range, metrics$count)
    })
    output$metrics <- shiny::renderTable({
      result <- ready(); shiny::req(result)
      data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                 Value = c(result$metrics$mad, result$metrics$mse, result$metrics$mape),
                 Observations = result$metrics$count)
    }, digits = 4, na = "N/A")
    forecast_frame <- shiny::reactive({
      result <- ready(); shiny::req(result)
      data.frame(Period = current()$data$labels, Actual = c(result$actual, NA_real_),
                 Forecast = c(result$fitted, result$forecast), Error = c(result$errors, NA_real_))
    })
    output$forecast_table <- DT::renderDT({
      frame <- forecast_frame()
      DT::formatRound(DT::datatable(frame, rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, order = list())), c("Actual", "Forecast", "Error"), 4)
    }, server = FALSE)
    output$forecast_plot <- shiny::renderPlot({
      frame <- forecast_frame()
      frame$Position <- seq_len(nrow(frame))
      ticks <- unique(as.integer(pretty(frame$Position, n = 8)))
      ticks <- ticks[ticks >= 1 & ticks <= nrow(frame)]
      ggplot2::ggplot(frame, ggplot2::aes(x = Position)) +
        ggplot2::geom_line(ggplot2::aes(y = Actual, colour = "Actual"), na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(y = Actual, colour = "Actual"), na.rm = TRUE) +
        ggplot2::geom_line(ggplot2::aes(y = Forecast, colour = "Forecast / fitted"), linetype = "dashed", na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(y = Forecast, colour = "Forecast / fitted"), shape = 17, na.rm = TRUE) +
        ggplot2::scale_x_continuous(breaks = ticks, labels = frame$Period[ticks]) +
        ggplot2::scale_colour_manual(values = c(Actual = "#222222", "Forecast / fitted" = "#2166AC")) +
        ggplot2::labs(x = "Period", y = current()$data$name, colour = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    })
    output$parameters <- shiny::renderTable({
      result <- ready(); shiny::req(result)
      data.frame(Setting = gsub("_", " ", names(result$parameters)),
        Value = vapply(result$parameters, function(value) paste(format(value, digits = 7, trim = TRUE), collapse = ", "), character(1)))
    }, rownames = FALSE)
    output$extra_details <- shiny::renderUI({
      result <- ready(); shiny::req(result)
      shiny::tagList(
        if (!is.null(result$details$search)) shiny::h4("Candidate windows on common evaluation periods"),
        if (!is.null(result$details$seasonal)) shiny::h4("Seasonal components"),
        if (!is.null(result$details$initialization)) {
          initial <- result$details$initialization
          shiny::tagList(shiny::p(sprintf("Initialization uses rows 1 to %d. States refer to period %d; subsequent forecasts use later observations only.", initial$count, initial$count)),
            if (length(initial$seasonal)) shiny::p(paste("Initial seasonal components (in observation order):", paste(signif(initial$seasonal, 6), collapse = ", "))))
        })
    })
    output$search_table <- shiny::renderTable({
      result <- ready(); shiny::req(result)
      result$details$search
    }, digits = 5)
    output$seasonal_table <- shiny::renderTable({
      result <- ready(); shiny::req(result)
      result$details$seasonal
    }, digits = 6)
    output$component_plot <- shiny::renderPlot({
      result <- ready(); shiny::req(result)
      components <- result$details$decomposition
      shiny::req(!is.null(components))
      columns <- setdiff(names(components), "Position")
      long <- do.call(rbind, lapply(columns, function(name) data.frame(Position = components$Position, Component = name, Value = components[[name]])))
      ggplot2::ggplot(long, ggplot2::aes(Position, Value)) + ggplot2::geom_line(na.rm = TRUE) +
        ggplot2::facet_wrap(~Component, scales = "free_y", ncol = 2) + ggplot2::theme_minimal() +
        ggplot2::labs(x = "Period", y = NULL)
    })
    output$regression_plot <- shiny::renderPlot({
      result <- ready(); shiny::req(result)
      initial <- result$details$initialization
      regression <- if (!is.null(initial)) initial$regression else result$details$regression
      shiny::req(!is.null(regression))
      observed <- if (!is.null(initial)) initial$initialization_data else result$details$decomposition$Adjusted
      old <- graphics::par(mfrow = c(1, 2)); on.exit(graphics::par(old))
      graphics::plot(seq_along(observed), observed, xlab = "Period", ylab = "Adjusted / initialization series", main = "Regression fit")
      graphics::lines(seq_along(observed), regression$fitted, col = "steelblue", lwd = 2)
      graphics::plot(regression$fitted, regression$residuals, xlab = "Fitted", ylab = "Residual", main = "Residuals versus fitted")
      graphics::abline(h = 0, lty = 2)
    })
    correlation_plot <- function(values, title) {
      values <- values[is.finite(values)]
      if (length(values) < 3L || stats::sd(values) < 1e-12) {
        graphics::plot.new(); graphics::text(0.5, 0.5, paste(title, "- insufficient variation or observations")); return()
      }
      lag <- min(20L, length(values) - 1L)
      old <- graphics::par(mfrow = c(1, 2)); on.exit(graphics::par(old))
      stats::acf(values, lag.max = lag, main = paste(title, "ACF"))
      stats::pacf(values, lag.max = lag, main = paste(title, "PACF"))
    }
    output$acf_original <- shiny::renderPlot({ result <- ready(); shiny::req(result); correlation_plot(result$actual, "Original series") })
    output$acf_transformed <- shiny::renderPlot({
      result <- ready(); shiny::req(result, result$details$transformed)
      correlation_plot(result$details$transformed, "Modelled / differenced series")
    })
    output$acf_residuals <- shiny::renderPlot({
      result <- ready(); shiny::req(result, result$details$residuals)
      correlation_plot(result$details$residuals, "Model residuals")
    })
    output$estimates <- shiny::renderTable({ result <- ready(); shiny::req(result); result$details$estimates }, digits = 6)
    output$intervals <- shiny::renderTable({ result <- ready(); shiny::req(result); result$details$intervals }, digits = 4)
    output$ljung_box <- shiny::renderText({
      result <- ready(); shiny::req(result)
      p <- result$details$ljung_box_p
      if (is.null(p) || !is.finite(p)) "Ljung-Box check: insufficient residual degrees of freedom or variation."
      else sprintf("Ljung-Box residual check: lag %d, p-value %.5f. This is a diagnostic, not a guarantee of model validity.", result$details$ljung_box_lag, p)
    })
    list(result = ready, current = current, outcome = outcome)
  })
}

forecasting_ui <- function(id) {
  ns <- shiny::NS(id)
  catalog <- pp_catalog()
  groups <- lapply(unique(catalog$group), function(group) {
    rows <- catalog[catalog$group == group, , drop = FALSE]
    methods <- lapply(seq_len(nrow(rows)), function(i) shiny::tabPanel(rows$label[i],
      forecasting_method_ui(ns(paste0("method_", rows$id[i])), rows$id[i])))
    shiny::tabPanel(group, do.call(shiny::tabsetPanel,
      c(list(id = ns(paste0("group_", match(group, unique(catalog$group))))), methods)))
  })
  do.call(shiny::tabsetPanel, c(list(id = ns("group")), groups))
}

forecasting_server <- function(id, data_bundle) {
  shiny::moduleServer(id, function(input, output, session) {
    catalog <- pp_catalog()
    modules <- lapply(catalog$id, function(method) forecasting_method_server(paste0("method_", method), method, data_bundle))
    stats::setNames(modules, catalog$id)
  })
}
