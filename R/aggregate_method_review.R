ap_method_choices <- list(
  "Heuristic plans" = c("Chase" = "chase", "Level" = "level", "Mixed" = "mixed"),
  "Optimization" = c("Linear programming" = "lp"),
  "Transportation" = c("North-West Corner" = "transport_northwest",
    "Cheapest Cell First" = "transport_least_cost", "Minimum Cost" = "transport_optimal")
)

ap_method_review_ui <- function(id) {
  ns <- shiny::NS(id)
  active <- "(input.method === 'disaggregation' ? input.source_method : input.method)"
  transport <- paste0("(", active, " || '').indexOf('transport_') === 0")
  lp <- paste0(active, " === 'lp'")
  overtime <- paste0(transport, " || (", lp, " && input.allow_overtime)")
  subcontract <- paste0(transport, " || (", lp, " && input.allow_subcontract)")
  shiny::tagList(
    shiny::hr(), shiny::h4("Planning methods"),
    shiny::selectInput(ns("method"), "Planning method",
      c(ap_method_choices, list("Disaggregation" = c("Square-root rule" = "disaggregation")))),
    shiny::uiOutput(ns("method_hint")),
    shiny::conditionalPanel("input.method === 'disaggregation'", ns = ns,
      shiny::selectInput(ns("source_method"), "Source plan for disaggregation", ap_method_choices),
      shiny::p("The square-root rule allocates in-house production. Double-click to enter each family's annual usage in planning units and setup cost per run. Leave minimum or maximum blank for no additional limit."),
      DT::DTOutput(ns("families")),
      shiny::actionButton(ns("add_family"), "Add family"),
      shiny::actionButton(ns("remove_family"), "Remove last family")),
    shiny::conditionalPanel(paste0(active, " === 'mixed'"), ns = ns,
      shiny::selectizeInput(ns("change_periods"), "Periods when workforce can change", choices = character(), multiple = TRUE),
      shiny::helpText("Workforce remains at the preceding level in other periods.")),
    shiny::conditionalPanel(lp, ns = ns,
      shiny::checkboxInput(ns("allow_overtime"), "Allow overtime"),
      shiny::checkboxInput(ns("allow_idle"), "Allow idle time"),
      shiny::checkboxInput(ns("allow_subcontract"), "Allow subcontracting"),
      shiny::conditionalPanel("input.allow_idle", ns = ns,
        shiny::numericInput(ns("idle_cost"), "Idle cost per unit of unused capacity", 0, min = 0))),
    shiny::conditionalPanel(transport, ns = ns,
      shiny::p("Regular capacity uses the starting workforce in every period. Unused capacity is separate from ending inventory.")),
    shiny::conditionalPanel(paste0("(", overtime, ") || (", subcontract, ")"), ns = ns,
      shiny::helpText("Enter one limit for every period or enter period limits in table order, separated by commas. Use a decimal point.")),
    shiny::conditionalPanel(overtime, ns = ns,
      shiny::textInput(ns("overtime_limit"), "Overtime capacity limit (planning units)", "0"),
      shiny::numericInput(ns("overtime_ratio"), "Overtime limit as a fraction of regular capacity", 1, min = 0),
      shiny::helpText("A fraction of 1 allows up to one additional regular workday's production per worker per day.")),
    shiny::conditionalPanel(subcontract, ns = ns,
      shiny::textInput(ns("subcontract_limit"), "Subcontract capacity limit (planning units)", "0")),
    shiny::actionButton(ns("run_plan"), "Run plan", class = "btn-primary"))
}

ap_review_limits <- function(value, n, label) {
  fields <- trimws(strsplit(paste0(value, ",end"), ",", fixed = TRUE)[[1L]])
  values <- suppressWarnings(as.numeric(head(fields, -1L)))
  if (!length(values) %in% c(1L, n) || any(!is.finite(values) | values < 0)) {
    pp_stop(sprintf("Enter one non-negative %s limit or %d limits in period order.", label, n))
  }
  rep_len(values, n)
}

ap_method_review_server <- function(id, data_bundle, period_data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$method_hint <- shiny::renderUI({
      shiny::req(input$method)
      pp_plan_hint(input$method)
    })
    empty_family <- data.frame(family = "", setup_cost = NA_real_, annual_usage = NA_real_,
                               minimum = NA_real_, maximum = NA_real_, stringsAsFactors = FALSE)
    family_data <- shiny::reactiveVal(rbind(transform(empty_family, family = "Family A"),
                                          transform(empty_family, family = "Family B")))
    shiny::observeEvent(input$add_family, family_data(rbind(family_data(), empty_family)))
    shiny::observeEvent(input$remove_family, {
      data <- family_data()
      if (nrow(data) > 1L) family_data(head(data, -1L))
    })
    shiny::observeEvent(input$families_cell_edit, {
      edit <- input$families_cell_edit
      data <- family_data()
      row <- edit$row
      column <- edit$col + 1L
      shiny::req(row >= 1L, row <= nrow(data), column >= 1L, column <= ncol(data))
      value <- trimws(edit$value)
      if (column > 1L) {
        numeric_value <- suppressWarnings(as.numeric(value))
        if (nzchar(value) && is.na(numeric_value)) {
          shiny::showNotification("Enter a number or leave the cell empty.", type = "error")
          return()
        }
        value <- numeric_value
      }
      data[row, column] <- value
      family_data(data)
    }, priority = 5)
    output$families <- DT::renderDT({
      input$families_cell_edit
      data <- family_data()
      names(data) <- c("Family", "Setup cost per run", "Annual usage", "Minimum", "Maximum")
      DT::datatable(data, rownames = FALSE, editable = "cell", selection = "none",
        options = list(ordering = FALSE, paging = FALSE, dom = "t"))
    }, server = FALSE)
    shiny::observe({
      periods <- tryCatch(period_data(), error = function(e) NULL)
      choices <- if (is.null(periods)) character() else stats::setNames(seq_len(nrow(periods)), periods$period)
      selected <- shiny::isolate(input$change_periods)
      shiny::updateSelectizeInput(session, "change_periods", choices = choices,
        selected = intersect(selected, unname(choices)))
    })
    request <- shiny::reactive({
      settings <- data_bundle()
      disaggregation <- identical(input$method, "disaggregation")
      method <- if (disaggregation) input$source_method else input$method
      lp <- identical(method, "lp")
      transport <- startsWith(method, "transport_")
      allow_overtime <- transport || (lp && isTRUE(input$allow_overtime))
      allow_idle <- lp && isTRUE(input$allow_idle)
      allow_subcontract <- transport || (lp && isTRUE(input$allow_subcontract))
      list(settings = settings, method = method,
        change_periods = if (identical(method, "mixed")) as.integer(input$change_periods),
        allow_overtime = allow_overtime, allow_idle = allow_idle, allow_subcontract = allow_subcontract,
        overtime_ratio = if (allow_overtime) input$overtime_ratio else 0,
        overtime_limit = if (allow_overtime) input$overtime_limit else "0",
        subcontract_limit = if (allow_subcontract) input$subcontract_limit else "0",
        idle_cost = if (allow_idle) input$idle_cost else 0,
        families = if (disaggregation) family_data())
    })
    result <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run_plan, {
      selected <- tryCatch(request(), error = identity)
      value <- if (inherits(selected, "error")) selected else tryCatch(ap_run_review(selected), error = identity)
      result(if (inherits(value, "error")) list(request = selected, error = conditionMessage(value)) else
        list(request = selected, value = value))
    }, ignoreInit = TRUE)
    current <- shiny::reactive({
      computed <- result()
      if (is.null(computed)) return(list(message = "Run a method to preview the calculation."))
      selected <- tryCatch(request(), error = identity)
      if (!identical(computed$request, selected)) return(list(message = "Inputs changed. Run the plan again."))
      computed
    })
    output$review_status <- shiny::renderUI({
      state <- current()
      if (!is.null(state$message)) return(shiny::p(state$message))
      if (!is.null(state$error)) return(shiny::div(class = "alert alert-warning", state$error))
      plan <- state$value$plan
      text <- sprintf("Plan cost %.2f %s. Ending inventory %.6g %s.", plan$total_cost,
        state$request$settings$currency, plan$ending_inventory, state$request$settings$unit)
      shiny::div(class = "alert alert-success", text)
    })
    ap_results_server(input, output, session, current)
    current
  })
}

ap_review_capacity <- function(request, settings) {
  n <- nrow(settings$periods)
  overtime <- ap_review_limits(request$overtime_limit, n, "overtime")
  subcontract <- ap_review_limits(request$subcontract_limit, n, "subcontracting")
  ratio <- request$overtime_ratio
  if (length(ratio) != 1L || !is.numeric(ratio) || !is.finite(ratio) || ratio < 0) {
    pp_stop("Enter a non-negative overtime capacity fraction.")
  }
  list(overtime = overtime, subcontract = subcontract, ratio = ratio)
}

ap_review_linear_plan <- function(request, settings = request$settings, workforce = NULL) {
  if (identical(request$method, "mixed")) {
    return(ap_linear_plan(settings, change_periods = request$change_periods, workforce = workforce))
  }
  capacity <- ap_review_capacity(request, settings)
  ap_linear_plan(settings, allow_overtime = request$allow_overtime, allow_idle = request$allow_idle,
    allow_subcontract = request$allow_subcontract, overtime_limit = capacity$overtime,
    subcontract_limit = capacity$subcontract, idle_cost = request$idle_cost,
    overtime_ratio = capacity$ratio, workforce = workforce)
}

ap_run_review <- function(request) {
  settings <- request$settings
  method <- request$method
  raw <- switch(method,
    chase = ap_chase_plan(settings),
    level = ap_level_plan(settings),
    mixed = ap_review_linear_plan(request),
    lp = ap_review_linear_plan(request), {
      capacity <- ap_review_capacity(request, settings)
      overtime <- pmin(capacity$overtime, capacity$ratio * settings$starting_workforce * settings$k * settings$periods$days)
      ap_transport_plan(settings, overtime_limit = overtime, subcontract_limit = capacity$subcontract,
        method = sub("^transport_", "", method))
    })
  plan <- ap_check_plan(settings, raw, idle_cost = request$idle_cost)
  split <- if (!is.null(request$families)) ap_disaggregate(plan$rows$production, request$families)
  rounding <- ap_round_plan(request, plan)
  rounded_split <- NULL
  rounded_split_error <- NULL
  if (!is.null(split) && rounding$status != "unchanged") {
    comparison <- if (!is.null(rounding$plan)) rounding$plan else rounding$candidate
    if (!is.null(comparison)) {
      rounded_split <- tryCatch(ap_disaggregate(comparison$rows$production, request$families), error = identity)
      if (inherits(rounded_split, "error")) {
        rounded_split_error <- conditionMessage(rounded_split)
        rounded_split <- NULL
      }
    }
  }
  list(plan = plan, rounding = rounding, disaggregation = split,
    rounded_disaggregation = rounded_split, rounded_disaggregation_error = rounded_split_error)
}
