pp_read_dataset <- function(path, filename, sheet = NULL, separator = "auto", decimal = ".") {
  extension <- tolower(tools::file_ext(filename))
  if (extension == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) pp_stop("The readxl package is required for Excel files.")
    data <- as.data.frame(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"),
                          check.names = FALSE)
  } else if (extension == "csv") {
    if (!decimal %in% c(".", ",")) pp_stop("Select a decimal mark.")
    if (separator == "auto") {
      lines <- readLines(path, n = 10L, warn = FALSE, encoding = "UTF-8")
      candidates <- c(",", ";", "\t")
      counts <- vapply(candidates, function(sep) {
        connection <- textConnection(lines)
        on.exit(close(connection))
        values <- tryCatch(utils::count.fields(connection, sep = sep, quote = '"'), error = function(e) 1L)
        values <- values[is.finite(values)]
        if (!length(values)) return(0)
        if (values[1] > 1L && all(values == values[1])) values[1] else if (all(values == 1L)) 1 else 0
      }, numeric(1))
      separator <- candidates[which.max(counts)]
    }
    if (separator == "tab") separator <- "\t"
    if (!separator %in% c(",", ";", "\t")) pp_stop("Select a supported CSV separator.")
    if (separator == decimal) pp_stop("The CSV separator and decimal mark must be different.")
    data <- utils::read.table(path, header = TRUE, sep = separator, dec = decimal,
                              quote = '"', comment.char = "", check.names = FALSE,
                              stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"),
                              fileEncoding = "UTF-8-BOM", fill = FALSE)
  } else pp_stop("Upload an .xlsx or .csv file.")
  if (!ncol(data)) pp_stop("The file contains no columns.")
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    pp_stop("Column names must be non-empty and unique. Correct the file headers.")
  }
  data
}

pp_index <- function(values, n, unit = "period") {
  if (is.null(values)) return(list(labels = as.character(seq_len(n + 1L)),
                                  order = seq_len(n), note = "Row order is used as the period order."))
  if (length(values) != n || anyNA(values)) pp_stop("The period column contains missing values.")
  dates <- inherits(values, c("Date", "POSIXt"))
  if (is.character(values) && all(grepl("^\\d{4}-\\d{2}-\\d{2}$", values))) {
    values <- as.Date(values)
    if (anyNA(values)) pp_stop("The period column contains invalid ISO dates.")
    dates <- TRUE
  }
  if (dates) {
    if (unit == "period") pp_stop("Select a calendar interval for the date/time period column.")
    labels <- if (inherits(values, "POSIXt") && unit == "hour") format(values, "%Y-%m-%d %H:%M") else format(values, "%Y-%m-%d")
    if (unit %in% c("month", "quarter", "year")) {
      year <- as.integer(format(values, "%Y"))
      month <- as.integer(format(values, "%m"))
      index <- if (unit == "year") year else if (unit == "quarter") year * 4L + (month - 1L) %/% 3L else year * 12L + month - 1L
      step <- 1
      next_index <- tail(index, 1L) + 1L
      next_label <- if (unit == "year") as.character(next_index) else if (unit == "quarter") {
        sprintf("%d Q%d", next_index %/% 4L, next_index %% 4L + 1L)
      } else sprintf("%d-%02d", next_index %/% 12L, next_index %% 12L + 1L)
    } else {
      index <- as.numeric(as.POSIXct(values, tz = "UTC"))
      step <- switch(unit, hour = 3600, day = 86400, week = 604800,
                     pp_stop("Select a supported calendar interval."))
      next_value <- as.POSIXct(tail(index, 1L) + step, origin = "1970-01-01", tz = "UTC")
      next_label <- format(next_value, if (unit == "hour") "%Y-%m-%d %H:%M" else "%Y-%m-%d", tz = "UTC")
    }
  } else {
    if (!is.numeric(values) || any(!is.finite(values))) pp_stop("The period column must contain numeric periods, Excel dates, or ISO dates (YYYY-MM-DD).")
    index <- as.numeric(values)
    labels <- as.character(values)
    step <- if (n >= 2L) stats::median(diff(sort(index))) else 1
    next_label <- as.character(tail(index, 1L) + step)
  }
  if (anyDuplicated(index)) pp_stop("Duplicate periods were found. Resolve duplicates before forecasting.")
  ordering <- order(index)
  if (!identical(ordering, seq_len(n))) pp_stop("Periods are not in ascending order. Use 'Sort by period' or correct the table.")
  if (n >= 2L && any(abs(diff(index) - step) > max(1e-8, abs(step) * 1e-7))) {
    pp_stop("Missing or irregular periods were found. Correct the period sequence; observations will not be silently skipped or filled.")
  }
  list(labels = c(labels, next_label), order = ordering, note = "")
}

data_management_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("Dataset"),
      shiny::fileInput(ns("file"), "Upload Excel or CSV", accept = c(".xlsx", ".csv")),
      shiny::uiOutput(ns("excel_sheet_ui")),
      shiny::tags$details(shiny::tags$summary("CSV import settings"),
        shiny::selectInput(ns("separator"), "CSV separator", c("Detect automatically" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "tab")),
        shiny::selectInput(ns("decimal"), "CSV decimal mark", c("Point (.)" = ".", "Comma (,)" = ","))),
      shiny::hr(),
      shiny::selectInput(ns("target"), "Target series", choices = character()),
      shiny::selectInput(ns("period_column"), "Period column", choices = c("Use row order" = "")),
      shiny::selectInput(ns("time_unit"), "Calendar interval (for dates)",
        c("Numeric periods / row order" = "period", "Hourly" = "hour", "Daily" = "day", "Weekly" = "week", "Monthly" = "month", "Quarterly" = "quarter", "Yearly" = "year")),
      shiny::hr(),
      shiny::actionButton(ns("add_row"), "Add row", icon = shiny::icon("plus")),
      shiny::br(), shiny::br(),
      shiny::actionButton(ns("delete_rows"), "Delete selected rows", icon = shiny::icon("trash")),
      shiny::br(), shiny::br(),
      shiny::actionButton(ns("sort_rows"), "Sort by period", icon = shiny::icon("sort")),
      shiny::p(class = "help-block", "Double-click a cell to edit. Empty target cells must be completed before forecasting.")
    ),
    shiny::mainPanel(shiny::h3("Data Management"), shiny::uiOutput(ns("status")),
                     DT::DTOutput(ns("table")))
  )
}

data_management_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(data = data.frame(Period = numeric(), Demand = numeric()),
                                revision = 0L, import_error = NULL, pending_delete = integer(),
                                pending_delete_revision = NA_integer_)
    upload_key <- shiny::reactive({
      if (is.null(input$file)) return(NULL)
      file <- input$file
      if (tolower(tools::file_ext(file$name)) == "xlsx") {
        list(file$datapath, input$sheet)
      } else list(file$datapath, input$separator, input$decimal)
    })
    output$excel_sheet_ui <- shiny::renderUI({
      if (is.null(input$file) || tolower(tools::file_ext(input$file$name)) != "xlsx") return(NULL)
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) character())
      if (!length(sheets)) return(shiny::p("Excel worksheets could not be read."))
      shiny::selectInput(session$ns("sheet"), "Excel worksheet", choices = sheets)
    })
    shiny::observeEvent(upload_key(), {
      shiny::req(input$file)
      if (tolower(tools::file_ext(input$file$name)) == "xlsx") shiny::req(input$sheet)
      result <- tryCatch(pp_read_dataset(input$file$datapath, input$file$name,
                         input$sheet, input$separator, input$decimal), error = identity)
      if (inherits(result, "error")) {
        rv$import_error <- paste("Import failed:", conditionMessage(result), "The previous table is retained; forecasting is blocked until a valid file is loaded.")
      } else {
        rv$data <- result
        rv$import_error <- NULL
      }
      rv$revision <- rv$revision + 1L
    }, ignoreNULL = TRUE)
    shiny::observeEvent(names(rv$data), {
      columns <- names(rv$data)
      selected <- shiny::isolate(input$period_column)
      if (is.null(selected) || !selected %in% columns) {
        candidates <- columns[tolower(columns) %in% c("period", "date", "month", "time")]
        selected <- if (length(candidates)) candidates[1] else ""
      }
      shiny::updateSelectInput(session, "period_column", choices = c("Use row order" = "", stats::setNames(columns, columns)), selected = selected)
    }, ignoreNULL = FALSE)
    shiny::observe({
      data <- rv$data
      numeric_columns <- names(data)[vapply(data, function(x) is.numeric(x) && !inherits(x, c("Date", "POSIXt")), logical(1))]
      candidates <- setdiff(numeric_columns, input$period_column)
      selected <- shiny::isolate(input$target)
      if (is.null(selected) || !selected %in% candidates) {
        demand <- candidates[tolower(candidates) %in% c("demand", "sales", "failures", "value")]
        selected <- if (length(demand)) demand[1] else if (length(candidates)) candidates[1] else character()
      }
      shiny::updateSelectInput(session, "target", choices = candidates, selected = selected)
    })
    output$table <- DT::renderDT({
      DT::datatable(rv$data, rownames = FALSE, editable = "cell", selection = "multiple",
                    options = list(pageLength = 12L, scrollX = TRUE, dom = "tip", order = list()))
    }, server = FALSE)
    shiny::observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      data <- rv$data
      row <- info$row
      column <- info$col + 1L
      if (!length(row) || row < 1L || row > nrow(data) || column < 1L || column > ncol(data)) return()
      original <- data[[column]]
      value <- info$value
      converted <- tryCatch({
        if (!nzchar(trimws(value))) NA else if (inherits(original, "Date")) as.Date(value) else if (inherits(original, "POSIXt")) as.POSIXct(value, tz = "UTC") else if (is.numeric(original)) as.numeric(value) else as.character(value)
      }, warning = function(w) NA, error = function(e) NA)
      if (nzchar(trimws(value)) && is.na(converted)) {
        shiny::showNotification("The cell value is incompatible with this column's type. The edit was not applied.", type = "error")
        rv$revision <- rv$revision + 1L
        return()
      }
      data[row, column] <- converted
      rv$data <- data
      rv$revision <- rv$revision + 1L
    })
    shiny::observeEvent(input$add_row, {
      data <- rv$data
      row <- data[NA_integer_, , drop = FALSE]
      row[] <- lapply(row, function(x) { x[] <- NA; x })
      period <- input$period_column
      if (is.null(period) || !nzchar(period)) period <- if ("Period" %in% names(data)) "Period" else ""
      if (nzchar(period) && is.numeric(data[[period]]) && !inherits(data[[period]], c("Date", "POSIXt"))) {
        existing <- data[[period]][is.finite(data[[period]])]
        row[[period]] <- if (length(existing)) max(existing) + 1 else 1
      }
      rv$data <- rbind(data, row)
      rownames(rv$data) <- NULL
      rv$revision <- rv$revision + 1L
    })
    shiny::observeEvent(input$delete_rows, {
      selected <- input$table_rows_selected
      if (!length(selected)) { shiny::showNotification("Select rows to delete.", type = "message"); return() }
      rv$pending_delete <- selected
      rv$pending_delete_revision <- rv$revision
      shiny::showModal(shiny::modalDialog(title = "Confirm row deletion",
        sprintf("Delete %d selected row(s)?", length(selected)),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete"), "Delete rows", class = "btn-danger"))))
    })
    shiny::observeEvent(input$confirm_delete, {
      shiny::removeModal()
      selected <- rv$pending_delete
      if (!identical(rv$pending_delete_revision, rv$revision)) {
        rv$pending_delete <- integer()
        shiny::showNotification("The dataset changed while confirmation was open. Select the rows again.", type = "warning")
        return()
      }
      if (length(selected)) {
        rv$data <- rv$data[-selected, , drop = FALSE]
        rv$revision <- rv$revision + 1L
        rv$pending_delete <- integer()
      }
    })
    shiny::observeEvent(input$sort_rows, {
      if (is.null(input$period_column) || !nzchar(input$period_column)) {
        shiny::showNotification("Select a period column before sorting.", type = "message"); return()
      }
      rv$data <- rv$data[order(rv$data[[input$period_column]], na.last = TRUE), , drop = FALSE]
      rv$revision <- rv$revision + 1L
    })
    bundle <- shiny::reactive({
      if (!is.null(rv$import_error)) pp_stop(rv$import_error)
      if (!nrow(rv$data)) pp_stop("Upload a dataset or add rows in Data Management.")
      if (is.null(input$target) || !input$target %in% names(rv$data)) pp_stop("Select a numeric target series in Data Management.")
      y <- pp_series(rv$data[[input$target]])
      values <- if (is.null(input$period_column) || !nzchar(input$period_column)) NULL else rv$data[[input$period_column]]
      index <- pp_index(values, length(y), input$time_unit)
      list(y = y, name = input$target, labels = index$labels, note = index$note,
           revision = rv$revision, period_column = input$period_column, time_unit = input$time_unit)
    })
    output$status <- shiny::renderUI({
      state <- tryCatch(bundle(), error = identity)
      if (inherits(state, "error")) return(shiny::div(class = "alert alert-info", conditionMessage(state)))
      shiny::div(class = "alert alert-success", sprintf("%s: %d observations ready. %s", state$name, length(state$y), state$note))
    })
    bundle
  })
}
