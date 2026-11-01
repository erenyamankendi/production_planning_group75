# =====================================================================
# Box-Jenkins Models: AR(p), MA(q), ARMA(p,q), ARIMA(p,d,q)
# Nahmias, Section 2.10
#   - ACF / PACF plots are used to identify the model.
#   - Parameters are estimated by maximum likelihood (stats::arima).
#   - In-sample one-step-ahead forecast: F_t = D_t - residual_t,
#     so e_t = F_t - D_t = -residual_t.
#   - "Find the best orders" chooses the orders with the lowest AIC.
# One file serves four tabs: box_jenkins_ui(id, type), type = AR/MA/ARMA/ARIMA.
# =====================================================================

bj_order <- function(type, p, d, q) {
  switch(type,
         AR    = c(p, 0, 0),
         MA    = c(0, 0, q),
         ARMA  = c(p, 0, q),
         ARIMA = c(p, d, q))
}

bj_fit <- function(D, order) {
  tryCatch(
    stats::arima(D, order = order, include.mean = (order[2] == 0), method = "ML"),
    error = function(e) tryCatch(
      stats::arima(D, order = order, include.mean = (order[2] == 0), method = "CSS"),
      error = function(e2) NULL))
}

# All candidate orders for the automatic search.
bj_candidates <- function(type, pmax, dmax, qmax) {
  g <- switch(type,
              AR    = expand.grid(p = 1:pmax, d = 0, q = 0),
              MA    = expand.grid(p = 0, d = 0, q = 1:qmax),
              ARMA  = expand.grid(p = 1:pmax, d = 0, q = 1:qmax),
              ARIMA = expand.grid(p = 0:pmax, d = 0:dmax, q = 0:qmax))
  g[rowSums(g) > 0, , drop = FALSE]
}

box_jenkins_ui <- function(id, type = "ARIMA") {
  ns <- NS(id)
  show_p <- type %in% c("AR", "ARMA", "ARIMA")
  show_q <- type %in% c("MA", "ARMA", "ARIMA")
  show_d <- type == "ARIMA"
  sidebarLayout(
    sidebarPanel(
      data_input_ui(ns),
      radioButtons(ns("mode"), "Model orders",
                   choices = c("Enter orders manually" = "manual",
                               "Find the best orders (lowest AIC)" = "auto")),
      conditionalPanel("input.mode == 'manual'", ns = ns,
        if (show_p) numericInput(ns("p"), "p (AR order)", 1, min = if (type == "ARIMA") 0 else 1, step = 1),
        if (show_d) numericInput(ns("d"), "d (differencing)", 1, min = 0, max = 2, step = 1),
        if (show_q) numericInput(ns("q"), "q (MA order)", 1, min = if (type == "ARIMA") 0 else 1, step = 1)),
      conditionalPanel("input.mode == 'auto'", ns = ns,
        if (show_p) numericInput(ns("pmax"), "Largest p to try", 3, min = 1, step = 1),
        if (show_d) numericInput(ns("dmax"), "Largest d to try", 2, min = 0, max = 2, step = 1),
        if (show_q) numericInput(ns("qmax"), "Largest q to try", 3, min = 1, step = 1)),
      numericInput(ns("lag_max"), "Number of lags in ACF / PACF", 12, min = 2, step = 1),
      common_settings_ui(ns, horizon = 6)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecasts",
          plotOutput(ns("forecast_plot"), height = "350px"),
          fluidRow(
            column(4, h4("Error measures"), tableOutput(ns("error_table"))),
            column(8, h4("Model"), tableOutput(ns("param_table")))),
          h4("Future forecasts (95% interval)"),
          tableOutput(ns("future_table")),
          h4("In-sample one-step-ahead forecasts"),
          tableOutput(ns("forecast_table"))),
        tabPanel("ACF / PACF",
          h4("Original series"),
          plotOutput(ns("acf_plot"), height = "300px"),
          conditionalPanel("output.differenced", ns = ns,
            h4("Differenced series"),
            plotOutput(ns("acf_diff_plot"), height = "300px")),
          h4("Residuals of the fitted model"),
          plotOutput(ns("acf_resid_plot"), height = "300px")),
        tabPanel("Parameter estimates",
          tableOutput(ns("coef_table")),
          conditionalPanel("input.mode == 'auto'", ns = ns,
            h4("Search results"),
            tableOutput(ns("search_table"))))
      )
    )
  )
}

box_jenkins_server <- function(id, type = "ARIMA") {
  moduleServer(id, function(input, output, session) {
    run <- setup_run(input, output, session)
    D <- run$D
    P <- run$P
    num <- function(x, default) if (is.null(x) || is.na(x)) default else x

    search <- reactive({
      req(P()$mode == "auto")
      d <- D()
      cand <- bj_candidates(type, num(P()$pmax, 0), num(P()$dmax, 0),
                            num(P()$qmax, 0))
      cand$AIC <- apply(cand, 1, function(o) {
        f <- bj_fit(d, as.numeric(o))
        if (is.null(f) || is.na(f$aic)) Inf else f$aic
      })
      cand[order(cand$AIC), ]
    })

    order_used <- reactive({
      if (P()$mode == "auto") {
        s <- search()
        validate(need(is.finite(s$AIC[1]), "No model could be fitted."))
        as.numeric(s[1, c("p", "d", "q")])
      } else {
        bj_order(type, num(P()$p, 0), num(P()$d, 0), num(P()$q, 0))
      }
    })

    fit <- reactive({
      o <- order_used()
      validate(need(length(D()) > sum(o) + 3, "Not enough data for this model."))
      f <- bj_fit(D(), o)
      validate(need(!is.null(f), "The model could not be estimated. Try other orders."))
      f
    })

    result <- reactive({
      d <- D(); f <- fit(); o <- order_used()
      F <- d - as.numeric(residuals(f))
      first <- o[1] + o[2] + 1
      if (first > 1) F[1:(first - 1)] <- NA
      pr <- predict(f, n.ahead = P()$horizon)
      future <- as.numeric(pr$pred)
      from <- eval_start(P()$eval_from, F)
      list(F = F, future = future, se = as.numeric(pr$se),
           err = forecast_errors(d, F, from), from = from)
    })

    model_name <- reactive({
      o <- order_used()
      switch(type, AR = sprintf("AR(%d)", o[1]), MA = sprintf("MA(%d)", o[3]),
             ARMA = sprintf("ARMA(%d,%d)", o[1], o[3]),
             ARIMA = sprintf("ARIMA(%d,%d,%d)", o[1], o[2], o[3]))
    })

    output$forecast_plot <- renderPlot({
      r <- result(); n <- length(D()); h <- length(r$future)
      plot_forecast(D(), r$F, r$future, model_name(),
                    extra_y = c(r$future + 1.96 * r$se, r$future - 1.96 * r$se))
      lines((n + 1):(n + h), r$future + 1.96 * r$se, col = "firebrick", lty = 3)
      lines((n + 1):(n + h), r$future - 1.96 * r$se, col = "firebrick", lty = 3)
    })
    output$error_table <- renderTable(errors_table(result()$err), digits = 4)
    output$param_table <- renderTable({
      f <- fit()
      param_table(Selection = selection_text(P()), Model = model_name(), `sigma^2` = f$sigma2,
                  `Log-likelihood` = f$loglik,
                  AIC = if (is.na(f$aic)) "-" else round(f$aic, 3),
                  `Errors computed from period` = result()$from)
    })
    output$coef_table <- renderTable({
      f <- fit()
      se <- sqrt(pmax(diag(f$var.coef), 0))
      data.frame(Parameter = names(coef(f)), Estimate = round(coef(f), 4),
                 `Std. error` = round(se, 4), `t value` = round(coef(f) / se, 3),
                 check.names = FALSE)
    })
    output$future_table <- renderTable({
      r <- result(); n <- length(D())
      data.frame(Period = as.integer(n + seq_along(r$future)),
                 Forecast = round(r$future, 3),
                 Lower = round(r$future - 1.96 * r$se, 3),
                 Upper = round(r$future + 1.96 * r$se, 3))
    })
    output$forecast_table <- renderTable(
      forecast_table(D(), result()$F), na = "")

    draw_acf_pacf <- function(x, lag_max, label) {
      op <- par(mfrow = c(1, 2)); on.exit(par(op))
      acf(x, lag.max = lag_max, main = paste("ACF -", label))
      pacf(x, lag.max = lag_max, main = paste("PACF -", label))
    }
    output$acf_plot <- renderPlot(draw_acf_pacf(D(), P()$lag_max, "original"))
    output$differenced <- reactive(order_used()[2] > 0)
    outputOptions(output, "differenced", suspendWhenHidden = FALSE)
    output$acf_diff_plot <- renderPlot({
      d <- order_used()[2]
      req(d > 0)
      draw_acf_pacf(diff(D(), differences = d), P()$lag_max,
                    paste0("differenced (d = ", d, ")"))
    })
    output$acf_resid_plot <- renderPlot(
      draw_acf_pacf(as.numeric(residuals(fit())), P()$lag_max, "residuals"))
    output$search_table <- renderTable(search(), digits = 3)

    report <- reactive({
      d <- D(); r <- result(); f <- fit(); nm <- model_name(); o <- order_used()
      lag_max <- P()$lag_max; n <- length(d); h <- length(r$future)
      se <- sqrt(pmax(diag(f$var.coef), 0))
      plots <- list(
        function() {
          plot_forecast(d, r$F, r$future, nm,
                        extra_y = c(r$future + 1.96 * r$se, r$future - 1.96 * r$se))
          lines((n + 1):(n + h), r$future + 1.96 * r$se, col = "firebrick", lty = 3)
          lines((n + 1):(n + h), r$future - 1.96 * r$se, col = "firebrick", lty = 3)
        },
        function() {
          series <- list(original = d)
          if (o[2] > 0) series[[paste0("differenced (d = ", o[2], ")")]] <-
            diff(d, differences = o[2])
          series$residuals <- as.numeric(residuals(f))
          par(mfrow = c(length(series), 2), mar = c(4, 4, 3, 1))
          for (nm_s in names(series)) {
            acf(series[[nm_s]], lag.max = lag_max, main = paste("ACF -", nm_s))
            pacf(series[[nm_s]], lag.max = lag_max, main = paste("PACF -", nm_s))
          }
        })
      tabs <- list(
        `Model` = param_table(Selection = selection_text(P()), Model = nm, `sigma^2` = f$sigma2, `Log-likelihood` = f$loglik,
                              AIC = if (is.na(f$aic)) "-" else round(f$aic, 3),
                              `Errors computed from period` = r$from),
        `Parameter estimates` = data.frame(Parameter = names(coef(f)),
                                           Estimate = round(coef(f), 4),
                                           `Std. error` = round(se, 4), check.names = FALSE),
        `Error measures` = errors_table(r$err),
        `Future forecasts` = data.frame(Period = n + seq_along(r$future),
                                        Forecast = round(r$future, 3),
                                        Lower = round(r$future - 1.96 * r$se, 3),
                                        Upper = round(r$future + 1.96 * r$se, 3)),
        `In-sample forecasts` = forecast_table(d, r$F))
      if (P()$mode == "auto") tabs$`Order search` <- search()
      list(title = paste("Box-Jenkins:", nm), plots = plots, tables = tabs)
    })
    register_downloads(output, report, paste0("box_jenkins_", tolower(type)))
  })
}
