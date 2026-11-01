# =====================================================================
# Shared helpers for all forecasting modules (Nahmias, Chapter 2)
#
# Notation (book, Section 2.5 - 2.6):
#   D_t  = observed demand in period t
#   F_t  = one-step-ahead forecast for period t (made in period t - 1)
#   e_t  = F_t - D_t
#   MAD  = (1/n) * sum |e_t|
#   MSE  = (1/n) * sum e_t^2
#   MAPE = [(1/n) * sum |e_t / D_t|] * 100
# =====================================================================


# ---------------------------------------------------------------------
# 1) Reading the data file (Excel or CSV)
# ---------------------------------------------------------------------

# Reads an uploaded .xlsx / .xls / .csv file into a data frame.
# CSV files saved by Turkish Excel use ";" and decimal ",", so read.csv2
# is tried when read.csv finds only one column.
read_data_file <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet) || sheet == "") sheet <- 1
    df <- readxl::read_excel(path, sheet = sheet)
  } else if (ext %in% c("csv", "txt")) {
    df <- utils::read.csv(path, check.names = FALSE)
    if (ncol(df) == 1) {
      df2 <- utils::read.csv2(path, check.names = FALSE)
      if (ncol(df2) > 1) df <- df2
    }
  } else {
    stop("Please upload an Excel (.xlsx, .xls) or CSV (.csv) file.")
  }
  as.data.frame(df)
}

# Converts a column to numbers ("12,5" is read as 12.5).
as_number <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

# Names of the columns that contain at least one number.
numeric_columns <- function(df) {
  names(df)[vapply(df, function(x) any(!is.na(as_number(x))), logical(1))]
}


# ---------------------------------------------------------------------
# 2) Shared data-upload block (UI + server part)
# ---------------------------------------------------------------------

data_input_ui <- function(ns) {
  tagList(
    uiOutput(ns("file_ui")),
    actionButton(ns("remove_file"), "Remove file", icon = icon("xmark"),
                 class = "btn-sm btn-outline-secondary"),
    br(), br(),
    uiOutput(ns("sheet_ui")),
    selectInput(ns("column"), "Series (demand column)", choices = NULL)
  )
}

# Call inside moduleServer.
# Returns list(D = reactive numeric vector, version = reactive counter).
# "Remove file" draws a new, empty file box (new id), so the old file is gone.
data_input_server <- function(input, output, session) {
  version <- reactiveVal(0)
  current_file <- reactive(input[[paste0("file", version())]])

  output$file_ui <- renderUI({
    fileInput(session$ns(paste0("file", version())), "Upload data (Excel or CSV)",
              accept = c(".xlsx", ".xls", ".csv"))
  })

  observeEvent(input$remove_file, {
    version(version() + 1)
    updateSelectInput(session, "column", choices = character(0))
  })

  output$sheet_ui <- renderUI({
    f <- current_file()
    req(f)
    ext <- tolower(tools::file_ext(f$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- readxl::excel_sheets(f$datapath)
    selectInput(session$ns("sheet"), "Sheet", choices = sheets)
  })

  raw_data <- reactive({
    f <- current_file()
    req(f)
    tryCatch(
      read_data_file(f$datapath, f$name, input$sheet),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })

  observeEvent(raw_data(), {
    cols <- numeric_columns(raw_data())
    # Prefer a column that is not just a period counter
    pick <- cols[!tolower(cols) %in% c("period", "t", "time", "week", "month",
                                       "quarter", "year", "day")]
    selected <- if (length(pick) > 0) pick[1] else cols[1]
    updateSelectInput(session, "column", choices = cols, selected = selected)
  })

  D <- reactive({
    validate(need(!is.null(current_file()), "Please upload a data file first."))
    req(input$column, input$column %in% names(raw_data()))
    D <- as_number(raw_data()[[input$column]])
    D <- D[!is.na(D)]
    validate(need(length(D) >= 3, "The selected column needs at least 3 numbers."))
    D
  })

  list(D = D, version = version)
}

# Run button logic. Nothing is computed until "Run" is pressed.
# On Run, the data and all settings are copied ("snapshot"); the results use
# only this copy, so changing a box later has no effect until Run is pressed
# again. Removing the file clears the results.
# Returns list(D = reactive data, P = reactive list of settings).
setup_run <- function(input, output, session) {
  data <- data_input_server(input, output, session)
  snapshot <- reactiveVal(NULL)

  observeEvent(input$run, {
    snap <- tryCatch(
      list(D = data$D(), p = reactiveValuesToList(input), error = NULL),
      error = function(e) {
        msg <- conditionMessage(e)
        if (!nzchar(msg)) msg <- "Please upload a file and choose the series first."
        list(error = msg)
      })
    snapshot(snap)
  })
  observeEvent(data$version(), snapshot(NULL), ignoreInit = TRUE)

  output$has_result <- reactive(!is.null(snapshot()) && is.null(snapshot()$error))
  outputOptions(output, "has_result", suspendWhenHidden = FALSE)

  get <- function() {
    s <- snapshot()
    validate(need(!is.null(s), "Upload a file, choose the settings and press Run."))
    validate(need(is.null(s$error), s$error))
    s
  }
  list(D = reactive(get()$D), P = reactive(get()$p))
}

run_and_download_ui <- function(ns) {
  tagList(
    actionButton(ns("run"), "Run", icon = icon("play"),
                 class = "btn-primary", width = "100%"),
    conditionalPanel("output.has_result", ns = ns,
      br(),
      downloadButton(ns("download_pdf"), "PDF report", class = "btn-sm"),
      downloadButton(ns("download_xlsx"), "Excel tables", class = "btn-sm"))
  )
}


# ---------------------------------------------------------------------
# Reports: PDF (plots + tables) and Excel (one sheet per table)
# A report is a list(title, plots = list of functions, tables = named list).
# ---------------------------------------------------------------------

# Turns a data frame into aligned text lines (empty cells instead of NA).
table_lines <- function(df) {
  df[] <- lapply(df, function(x) {
    y <- if (is.numeric(x)) format(round(x, 4), trim = TRUE, drop0trailing = TRUE)
         else as.character(x)
    y[is.na(x)] <- ""
    y
  })
  capture.output(print(df, row.names = FALSE, right = TRUE))
}

# All tables as one list of lines with headings; heading lines are marked.
report_lines <- function(tables) {
  out <- character(0); bold <- logical(0)
  for (nm in names(tables)) {
    tl <- table_lines(tables[[nm]])
    out <- c(out, nm, tl, ""); bold <- c(bold, TRUE, rep(FALSE, length(tl)), FALSE)
  }
  list(text = out, bold = bold)
}

# Writes lines top-down in the current plot region.
draw_lines <- function(text, bold, n_slots, cex = 0.8) {
  par(mar = c(0.5, 1, 0.5, 1))
  plot.new()
  if (length(text) == 0) return(invisible())
  y <- 1 - (seq_along(text) - 0.5) / n_slots
  if (any(bold))
    text(0, y[bold], text[bold], adj = c(0, 0.5), cex = cex * 1.1, font = 2)
  if (any(!bold))
    text(0, y[!bold], text[!bold], adj = c(0, 0.5), cex = cex, family = "mono")
}

# PDF (A4): page 1 = title, main plot, parameters and error measures.
# Then the other plots (two per page) and the other tables.
write_pdf_report <- function(file, rep) {
  pdf(file, width = 8.27, height = 11.69)
  on.exit(dev.off())
  slots <- 66

  first <- intersect(names(rep$tables), c("Parameters", "Model", "Error measures"))
  rest  <- setdiff(names(rep$tables), first)

  # Page 1
  layout(matrix(1:2), heights = c(1.15, 1))
  par(oma = c(0, 0, 3, 0), mar = c(5, 4, 3, 2))
  rep$plots[[1]]()
  mtext(rep$title, side = 3, outer = TRUE, line = 1.4, adj = 0, font = 2, cex = 1.3)
  mtext(paste("Production Planning - created", format(Sys.time(), "%Y-%m-%d %H:%M")),
        side = 3, outer = TRUE, line = 0.2, adj = 0, cex = 0.8)
  l1 <- report_lines(rep$tables[first])
  draw_lines(l1$text, l1$bold, n_slots = 30)

  # Other plots (search curve, ACF / PACF): one page each, upper part
  for (p in rep$plots[-1]) {
    layout(matrix(1:2), heights = c(1, 0.6)); par(oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2))
    p()
    layout(1); par(mfrow = c(1, 1))
  }

  # Other tables, as many lines per page as fit
  if (length(rest) > 0) {
    lr <- report_lines(rep$tables[rest])
    pages <- split(seq_along(lr$text), ceiling(seq_along(lr$text) / slots))
    for (idx in pages) {
      layout(1); par(oma = c(1, 1, 1, 1))
      draw_lines(lr$text[idx], lr$bold[idx], n_slots = slots)
    }
  }
}

write_xlsx_report <- function(file, rep) {
  sheets <- rep$tables
  names(sheets) <- substr(gsub("[^A-Za-z0-9 ()_-]", "", names(sheets)), 1, 31)
  writexl::write_xlsx(sheets, file)
}

# Registers the two download buttons. `report` is a reactive report list.
register_downloads <- function(output, report, file_prefix) {
  output$download_pdf <- downloadHandler(
    filename = function() paste0(file_prefix, "_", Sys.Date(), ".pdf"),
    content = function(file) write_pdf_report(file, report()))
  output$download_xlsx <- downloadHandler(
    filename = function() paste0(file_prefix, "_", Sys.Date(), ".xlsx"),
    content = function(file) write_xlsx_report(file, report()))
}


# ---------------------------------------------------------------------
# 3) Error measures
# ---------------------------------------------------------------------

# Error measures over periods `from` ... n (only where a forecast exists).
forecast_errors <- function(D, F, from = 1) {
  n <- length(D)
  from <- max(1, from)
  t <- from:n
  t <- t[!is.na(F[t])]
  if (length(t) == 0) return(c(MAD = NA, MSE = NA, MAPE = NA))
  e <- F[t] - D[t]
  ok <- D[t] != 0
  c(MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = mean(abs(e[ok] / D[t][ok])) * 100)
}

# First period for error measures: user's choice, or the first forecast.
eval_start <- function(user_from, F) {
  first_fc <- which(!is.na(F))[1]
  if (is.null(user_from) || is.na(user_from)) return(first_fc)
  max(first_fc, user_from)
}

errors_table <- function(err) {
  data.frame(Measure = names(err), Value = round(unname(err), 4))
}


# ---------------------------------------------------------------------
# 4) Regression (Section 2.8) - used for initial level and slope
# ---------------------------------------------------------------------

# Fits D_t = a + b t to periods 1..k (book formulas, Section 2.8).
regression_fit <- function(D, k = length(D)) {
  k <- min(k, length(D))
  d <- D[1:k]
  i <- 1:k
  Sxy <- k * sum(i * d) - k * (k + 1) / 2 * sum(d)
  Sxx <- k^2 * (k + 1) * (2 * k + 1) / 6 - k^2 * (k + 1)^2 / 4
  b <- Sxy / Sxx
  a <- mean(d) - b * (k + 1) / 2
  list(a = a, b = b)
}


# ---------------------------------------------------------------------
# 5) Common screen pieces
# ---------------------------------------------------------------------

common_settings_ui <- function(ns, horizon = 4) {
  tagList(
    hr(),
    numericInput(ns("horizon"), "Number of future periods to forecast",
                 value = horizon, min = 1, step = 1),
    numericInput(ns("eval_from"),
                 "Compute MAD / MSE / MAPE from period (empty = first forecast)",
                 value = NA, min = 1, step = 1),
    hr(),
    run_and_download_ui(ns)
  )
}

criterion_ui <- function(ns) {
  selectInput(ns("criterion"), "Choose the best value by",
              choices = c("MSE", "MAD", "MAPE"))
}

results_ui <- function(ns, extra_top = NULL) {
  tagList(
    extra_top,
    plotOutput(ns("forecast_plot"), height = "350px"),
    fluidRow(
      column(4, h4("Error measures"), tableOutput(ns("error_table"))),
      column(8, h4("Parameters used"), tableOutput(ns("param_table")))
    ),
    h4("Forecasts"),
    tableOutput(ns("forecast_table"))
  )
}

# Plot of demand, in-sample forecasts and future forecasts.
plot_forecast <- function(D, F, future = NULL, title = "",
                          fitted_label = "One-step-ahead forecast", extra_y = NULL) {
  n <- length(D)
  h <- length(future)
  x_all <- 1:(n + h)
  y_all <- c(D, F, future, extra_y)
  plot(x_all, rep(NA, n + h), ylim = range(y_all, na.rm = TRUE),
       xlab = "Period", ylab = "Demand", main = title, type = "n")
  grid()
  lines(1:n, D, type = "o", pch = 16, col = "black", lwd = 2)
  lines(1:n, F, type = "o", pch = 1, col = "steelblue", lwd = 2)
  if (h > 0) {
    lines((n + 1):(n + h), future, type = "o", pch = 17, col = "firebrick",
          lwd = 2, lty = 2)
    abline(v = n + 0.5, col = "grey60", lty = 3)
  }
  legend("topleft", bty = "n",
         legend = c("Demand", fitted_label, "Future forecast"),
         col = c("black", "steelblue", "firebrick"),
         pch = c(16, 1, 17), lty = c(1, 1, 2), lwd = 2)
}

# Table with Period, Demand, Forecast, Error (+ future rows).
forecast_table <- function(D, F, future = NULL, digits = 3) {
  n <- length(D)
  h <- length(future)
  data.frame(
    Period   = as.integer(1:(n + h)),
    Demand   = c(D, rep(NA, h)),
    Forecast = round(c(F, future), digits),
    Error    = round(c(F - D, rep(NA, h)), digits),
    check.names = FALSE
  )
}

# Says whether the parameters were typed in or found by the search.
selection_text <- function(p) {
  if (!is.null(p$mode) && p$mode == "auto") {
    crit <- if (is.null(p$criterion)) "AIC" else p$criterion
    paste0("Best value found by search (lowest ", crit, ")")
  } else "Entered manually"
}

# Parameter table (name / value pairs).
param_table <- function(...) {
  p <- list(...)
  data.frame(Parameter = names(p),
             Value = vapply(p, function(v) {
               if (is.numeric(v)) format(round(v, 4), nsmall = 0) else as.character(v)
             }, character(1)))
}

# Grid of smoothing constants for the "find the best value" searches.
smoothing_grid <- function(step = 0.01) seq(step, 1, by = step)
