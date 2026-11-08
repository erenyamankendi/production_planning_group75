ap_cost_labels <- c(hiring = "Hiring cost per worker", firing = "Firing cost per worker",
                    holding = "Holding cost per planning unit per period",
                    regular = "Base production cost per planning unit",
                    overtime = "Overtime premium per planning unit",
                    subcontract = "Subcontracting cost per planning unit")

aggregate_planning_ui <- function(id) {
  ns <- shiny::NS(id)
  layout <- shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h4("Planning data"),
      shiny::fileInput(ns("file"), "Upload Excel or CSV", accept = c(".xlsx", ".csv")),
      shiny::uiOutput(ns("worksheet_ui")),
      shiny::tags$details(shiny::tags$summary("CSV import settings"),
        shiny::selectInput(ns("separator"), "CSV separator",
          c("Detect automatically" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "tab")),
        shiny::selectInput(ns("decimal"), "CSV decimal mark", c("Point (.)" = ".", "Comma (,)" = ","))),
      shiny::uiOutput(ns("columns_ui")),
      shiny::hr(),
      shiny::h4("Units"),
      shiny::textInput(ns("demand_unit"), "Demand unit in file", placeholder = "e.g. boxes"),
      shiny::textInput(ns("planning_unit"), "Planning quantity unit", placeholder = "e.g. pieces"),
      shiny::uiOutput(ns("demand_conversion_ui")),
      shiny::textInput(ns("days_unit"), "Working time unit in file", placeholder = "e.g. days"),
      shiny::uiOutput(ns("days_conversion_ui")),
      shiny::textInput(ns("currency"), "Cost currency", placeholder = "e.g. TRY"),
      shiny::helpText("Review the imported units and fill in any missing units. Use the planning quantity unit for stock, costs and productivity.")),
    shiny::mainPanel(
      shiny::h3("Aggregate Planning"),
      pp_presentation_hint("Planning flow", "Load demand and working days, enter costs and stock targets, then choose a method below."),
      shiny::uiOutput(ns("status")),
      shiny::h4("Period preview"),
      shiny::p("Double-click a period name to edit it. The row order is the planning order."),
      DT::DTOutput(ns("period_preview")),
      shiny::fluidRow(
        shiny::column(6,
          shiny::h4("Workforce and inventory"),
          shiny::numericInput(ns("starting_workforce"), "Starting workforce (workers)", NA_real_, min = 0, step = 1),
          shiny::numericInput(ns("starting_inventory"), "Starting inventory (planning units)", NA_real_, min = 0),
          shiny::numericInput(ns("ending_inventory"), "Ending inventory target (planning units)", NA_real_, min = 0),
          shiny::radioButtons(ns("ending_mode"), "Ending inventory rule",
            c("Minimum" = "minimum", "Exact" = "exact")),
          shiny::h4("Productivity"),
          shiny::radioButtons(ns("productivity_mode"), "Productivity source",
            c("Enter K directly" = "manual", "Calculate from historical data" = "historical")),
          shiny::conditionalPanel("input.productivity_mode === 'manual'", ns = ns,
            shiny::numericInput(ns("productivity"), "K (planning units per worker per day)", NA_real_, min = 0)),
          shiny::conditionalPanel("input.productivity_mode === 'historical'", ns = ns,
            shiny::numericInput(ns("past_production"), "Historical production (planning units)", NA_real_, min = 0),
            shiny::numericInput(ns("past_workers"), "Historical workforce (workers)", NA_real_, min = 1, step = 1),
            shiny::numericInput(ns("past_days"), "Historical working days", NA_real_, min = 1, step = 1)),
          shiny::textOutput(ns("productivity_summary"))),
        shiny::column(6,
          shiny::h4("Costs"),
          shiny::p("Enter costs in the selected currency. Enter zero for costs that do not apply. The overtime premium is added to the base production cost."),
          lapply(names(ap_cost_labels), function(field) {
            shiny::numericInput(ns(field), ap_cost_labels[[field]], NA_real_, min = 0)
          }))),
      ap_method_review_ui(ns("methods"))))
  shiny::tagList(layout, ap_results_ui(shiny::NS(ns("methods"))))
}

aggregate_planning_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    unit_text <- function(value) ap_normalize_unit(if (is.null(value)) "" else value, explicit = TRUE)
    number <- function(value, label, positive = FALSE, whole = FALSE) {
      if (length(value) != 1L || !is.numeric(value) || !is.finite(value) || value < 0 ||
          (positive && value == 0) || (whole && value != floor(value))) {
        requirement <- if (whole) "a non-negative whole number" else if (positive) "a positive number" else "a non-negative number"
        pp_stop(sprintf("Enter %s for %s.", requirement, label))
      }
      value
    }
    is_excel <- shiny::reactive({
      !is.null(input$file) && tolower(tools::file_ext(input$file$name)) == "xlsx"
    })
    worksheets <- shiny::reactive({
      if (!is_excel()) return(character())
      tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) character())
    })
    output$worksheet_ui <- shiny::renderUI({
      sheets <- worksheets()
      if (length(sheets)) shiny::selectInput(session$ns("worksheet"), "Excel worksheet", sheets)
    })
    imported <- shiny::reactive({
      if (is.null(input$file)) return(NULL)
      file <- input$file
      tryCatch({
        if (is_excel()) {
          sheets <- worksheets()
          if (!length(sheets)) pp_stop("The Excel worksheets could not be read.")
          sheet <- input$worksheet
          if (is.null(sheet) || !sheet %in% sheets) sheet <- sheets[1L]
          pp_read_dataset(file$datapath, file$name, sheet = sheet)
        } else {
          pp_read_dataset(file$datapath, file$name, separator = input$separator, decimal = input$decimal)
        }
      }, error = identity)
    })
    output$columns_ui <- shiny::renderUI({
      data <- imported()
      if (!is.data.frame(data)) return(NULL)
      guesses <- ap_guess_columns(data)
      choices <- c("Select a column" = "", stats::setNames(names(data), names(data)))
      labels <- c(period = "Period column", demand = "Demand forecast column", days = "Working time column")
      lapply(names(labels), function(field) {
        shiny::selectInput(session$ns(paste0(field, "_column")), labels[[field]], choices,
                          selected = guesses[[field]], selectize = FALSE)
      })
    })
    shiny::observeEvent(list(imported(), input$demand_column), {
      data <- imported()
      shiny::req(is.data.frame(data), input$demand_column %in% names(data))
      unit <- ap_detect_units(data, input$demand_column, "")[["demand"]]
      shiny::updateTextInput(session, "demand_unit", value = unit)
      shiny::updateTextInput(session, "planning_unit", value = unit)
    })
    shiny::observeEvent(list(imported(), input$days_column), {
      data <- imported()
      shiny::req(is.data.frame(data), input$days_column %in% names(data))
      unit <- ap_detect_units(data, "", input$days_column)[["days"]]
      shiny::updateTextInput(session, "days_unit", value = unit)
    })
    demand_conversion <- shiny::reactive({
      source <- unit_text(input$demand_unit)
      target <- unit_text(input$planning_unit)
      nzchar(source) && nzchar(target) && tolower(source) != tolower(target)
    })
    days_conversion <- shiny::reactive({
      unit <- unit_text(input$days_unit)
      nzchar(unit) && unit != "days"
    })
    output$demand_conversion_ui <- shiny::renderUI({
      if (demand_conversion()) {
        shiny::numericInput(session$ns("demand_factor"),
          sprintf("%s per 1 %s", unit_text(input$planning_unit), unit_text(input$demand_unit)), NA_real_, min = 0)
      }
    })
    output$days_conversion_ui <- shiny::renderUI({
      if (days_conversion()) {
        shiny::numericInput(session$ns("days_factor"),
          sprintf("Working days per 1 %s", unit_text(input$days_unit)), NA_real_, min = 0)
      }
    })
    renamed <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(imported(), input$period_column), renamed(NULL), priority = 10)
    periods <- shiny::reactive({
      data <- imported()
      if (is.null(data)) pp_stop("Upload a planning dataset to start.")
      if (inherits(data, "error")) pp_stop(paste("Import failed.", conditionMessage(data)))
      columns <- c(input$period_column, input$demand_column, input$days_column)
      if (length(columns) != 3L || any(!nzchar(columns))) pp_stop("Select the period, demand forecast and working time columns.")
      if (!all(nzchar(c(unit_text(input$demand_unit), unit_text(input$planning_unit), unit_text(input$days_unit))))) {
        pp_stop("Review the demand, planning quantity and working time units before previewing the periods.")
      }
      day_factor <- if (days_conversion()) number(input$days_factor, "the working time conversion", positive = TRUE) else 1
      demand_factor <- if (demand_conversion()) number(input$demand_factor, "the demand conversion", positive = TRUE) else 1
      decimal <- if (is_excel()) "." else input$decimal
      result <- ap_prepare_periods(data, columns[1], columns[2], columns[3], decimal, days_factor = day_factor)
      labels <- renamed()
      if (!is.null(labels)) result$period <- labels
      result$demand <- result$demand * demand_factor
      if (any(!is.finite(result$demand))) pp_stop("The demand conversion produces values outside the numeric range.")
      result
    })
    k <- shiny::reactive({
      if (identical(input$productivity_mode, "historical")) {
        return(ap_productivity(input$past_production, input$past_workers, input$past_days))
      }
      number(input$productivity, "K", positive = TRUE)
    })
    output$productivity_summary <- shiny::renderText({
      if (identical(input$productivity_mode, "historical")) {
        result <- tryCatch(k(), error = function(e) NULL)
        if (!is.null(result)) sprintf("Calculated K = %.8g planning units per worker per day", result)
      }
    })
    bundle <- shiny::reactive({
      data <- periods()
      currency <- trimws(if (is.null(input$currency)) "" else input$currency)
      if (!nzchar(currency)) pp_stop("Enter the currency used for the cost inputs.")
      if (length(input$ending_mode) != 1L || !input$ending_mode %in% c("minimum", "exact")) {
        pp_stop("Select an ending inventory rule.")
      }
      costs <- vapply(names(ap_cost_labels), function(field) {
        number(input[[field]], tolower(ap_cost_labels[[field]]))
      }, numeric(1), USE.NAMES = TRUE)
      list(periods = data, unit = unit_text(input$planning_unit), currency = currency, costs = costs,
        starting_workforce = number(input$starting_workforce, "starting workforce", whole = TRUE),
        starting_inventory = number(input$starting_inventory, "starting inventory"),
        ending_inventory = number(input$ending_inventory, "ending inventory"), ending_mode = input$ending_mode,
        k = k())
    })
    output$status <- shiny::renderUI({
      state <- tryCatch(bundle(), error = identity)
      if (inherits(state, "error")) return(shiny::div(class = "alert alert-info", conditionMessage(state)))
      shiny::div(class = "alert alert-success", sprintf("Planning inputs ready for %d periods.", nrow(state$periods)))
    })
    output$period_preview <- DT::renderDT({
      input$period_preview_cell_edit
      data <- tryCatch(periods(), error = function(e) NULL)
      shiny::req(!is.null(data))
      names(data) <- c("Period", sprintf("Demand (%s)", unit_text(input$planning_unit)), "Working days")
      DT::datatable(data, rownames = FALSE, editable = list(target = "cell", disable = list(columns = 1:2)),
                    options = list(ordering = FALSE, pageLength = 12L, dom = "tip"))
    }, server = FALSE)
    shiny::observeEvent(input$period_preview_cell_edit, {
      data <- tryCatch(periods(), error = function(e) NULL)
      edit <- input$period_preview_cell_edit
      shiny::req(!is.null(data), edit$col == 0L, edit$row >= 1L, edit$row <= nrow(data))
      name <- trimws(edit$value)
      if (!nzchar(name) || tolower(name) %in% tolower(data$period[-edit$row])) {
        shiny::showNotification("Enter a non-empty, unique period name.", type = "error")
        return()
      }
      data$period[edit$row] <- name
      renamed(data$period)
    }, priority = 5)
    ap_method_review_server("methods", data_bundle = bundle, period_data = periods)
    bundle
  })
}
