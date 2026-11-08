ap_results_ui <- function(ns) {
  shiny::tagList(shiny::h4("Plan results"), shiny::uiOutput(ns("review_status")),
    shiny::uiOutput(ns("plan_results")))
}

ap_results_datatable <- function(data, cost = FALSE) {
  names(data) <- make.unique(names(data))
  paging <- nrow(data) > 12L
  table <- DT::datatable(data, rownames = FALSE, selection = "none", class = "compact stripe",
    options = list(dom = if (paging) "tip" else "t", paging = paging, pageLength = 12L,
                   scrollX = TRUE, ordering = FALSE))
  numbers <- which(vapply(data, is.numeric, logical(1)))
  if (cost) {
    table <- DT::formatRound(table, numbers, digits = 2, dec.mark = ".")
    return(DT::formatStyle(table, "Cost item", target = "row", fontWeight = DT::styleEqual("Total cost", "bold")))
  }
  DT::formatSignif(table, numbers, digits = 7, dec.mark = ".")
}

ap_results_plot <- function(plans, unit) {
  series <- c("In-house production", "Net demand", "Total supply including subcontracting")
  colors <- stats::setNames(c("#236681", "#A33D2B", "#42672D"), series)
  line_types <- stats::setNames(c("solid", "longdash", "dotted"), series)
  periods <- plans[[1L]]$rows$period
  positions <- unique(round(seq(1, length(periods), length.out = min(8L, length(periods)))))
  labels <- periods[positions]
  labels <- ifelse(nchar(labels, type = "width") > 18L, paste0(strtrim(labels, 15L), "..."), labels)
  data <- do.call(rbind, lapply(names(plans), function(name) {
    rows <- plans[[name]]$rows
    values <- list(cumsum(rows$production), rows$cumulative_net_demand)
    if (any(rows$subcontract > 0)) values[[3L]] <- rows$cumulative_supply
    data.frame(position = seq_len(nrow(rows)), quantity = unlist(values, use.names = FALSE),
      series = rep(series[seq_along(values)], each = nrow(rows)), plan = name)
  }))
  data$plan <- factor(data$plan, levels = names(plans))
  data$series <- factor(data$series, levels = series)
  plot <- ggplot2::ggplot(data, ggplot2::aes(position, quantity, color = series, linetype = series))
  if (length(periods) > 1L) plot <- plot + ggplot2::geom_line(linewidth = 0.85)
  plot <- plot + ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = colors) + ggplot2::scale_linetype_manual(values = line_types) +
    ggplot2::scale_x_continuous(breaks = positions, labels = labels) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = "Period", y = sprintf("Cumulative quantity (%s)", unit), color = NULL, linetype = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom", strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  if (length(plans) > 1L) plot <- plot + ggplot2::facet_wrap(~plan, ncol = 1)
  plot
}

ap_results_server <- function(input, output, session, current) {
  comparison <- shiny::reactive({
    state <- current()
    shiny::req(state$value)
    rounding <- state$value$rounding
    candidate <- if (identical(rounding$status, "feasible")) rounding$plan else rounding$candidate
    rounded_name <- if (identical(rounding$status, "infeasible")) "Infeasible whole-worker candidate" else "Whole-worker plan"
    plans <- list("Original plan" = state$value$plan)
    if (!is.null(candidate) && !identical(rounding$status, "unchanged")) plans[[rounded_name]] <- candidate
    list(plans = plans, rounded_name = rounded_name, value = state$value, settings = state$request$settings)
  })

  plan_data <- function(plans, index) {
    labels <- c(period = "Period", workforce = "Workers", hired = "Hired", fired = "Fired",
      demand = "Demand", production = "In-house production", regular = "Regular production",
      overtime = "Overtime", subcontract = "Subcontracting", idle = "Idle capacity", inventory = "Inventory")
    used <- function(field) any(vapply(plans, function(plan) any(plan$rows[[field]] > 0), logical(1)))
    fields <- c("period", "workforce", "hired", "fired", "demand", "production",
      if (used("overtime")) c("regular", "overtime"), if (used("subcontract")) "subcontract",
      if (used("idle")) "idle", "inventory")
    stats::setNames(plans[[index]]$rows[fields], unname(labels[fields]))
  }

  split_data <- function(split, plan, currency) {
    setup <- split$annual_setup_cost
    setup_label <- sprintf("Annual setup cost (%s)", currency)
    data.frame(Period = plan$rows$period, split$allocations, stats::setNames(list(setup), setup_label), check.names = FALSE)
  }

  transport_data <- function(allocation, unit) {
    if (ncol(allocation) <= 15L) {
      return(data.frame("Supply source" = rownames(allocation), allocation, check.names = FALSE))
    }
    cells <- which(allocation > 0, arr.ind = TRUE)
    cells <- cells[order(cells[, 1L], cells[, 2L]), , drop = FALSE]
    data.frame("Supply source" = rownames(allocation)[cells[, 1L]],
      Destination = colnames(allocation)[cells[, 2L]],
      stats::setNames(list(allocation[cells]), sprintf("Allocated quantity (%s)", unit)), check.names = FALSE)
  }

  output$plan_results <- shiny::renderUI({
    if (is.null(current()$value)) return(NULL)
    info <- comparison()
    ns <- session$ns
    rounding <- info$value$rounding
    plans <- info$plans
    original <- plans[[1L]]
    rounded <- if (length(plans) > 1L) plans[[2L]]
    show_split <- !is.null(info$value$disaggregation)
    rounded_split <- info$value[["rounded_disaggregation"]]
    show_rounded_split <- !is.null(rounded_split) && !is.null(rounded)
    two_tables <- function(first, second = NULL, heading = NULL, rounded_heading = NULL) {
      shiny::fluidRow(
        shiny::column(if (is.null(second)) 12 else 6, shiny::h5("Original plan"), heading, DT::DTOutput(ns(first))),
        if (!is.null(second)) shiny::column(6, shiny::h5(info$rounded_name), rounded_heading, DT::DTOutput(ns(second))))
    }
    ending <- function(plan) {
      shiny::p(sprintf("Ending inventory %.7g %s", plan$ending_inventory, info$settings$unit))
    }
    split_notes <- function(split) {
      notes <- unique(split$notes[nzchar(split$notes)])
      if (length(notes)) shiny::p(class = "text-muted", paste(notes, collapse = " "))
    }
    shiny::tagList(
      shiny::h4("Period plans"),
      shiny::p(sprintf("Production, demand, inventory and capacity are in %s.", info$settings$unit)),
      if (identical(rounding$status, "infeasible")) {
        shiny::div(class = "alert alert-warning", shiny::strong("Whole-worker plan is infeasible. "), rounding$message)
      } else if (length(rounding$message) && nzchar(rounding$message)) {
        shiny::p(class = "text-muted", rounding$message)
      },
      two_tables("original_plan", if (!is.null(rounded)) "rounded_plan", ending(original),
        if (!is.null(rounded)) ending(rounded)),
      shiny::h4("Cost comparison"), DT::DTOutput(ns("plan_costs")),
      shiny::h4("Cumulative production and net demand"),
      shiny::plotOutput(ns("plan_plot"), height = if (is.null(rounded)) "365px" else "620px", fill = FALSE),
      shiny::p(class = "text-muted", "Net demand subtracts starting inventory and adds the ending inventory target to the last period."),
      if (!is.null(original$allocation)) shiny::tagList(
        shiny::h4("Transportation allocation"),
        shiny::p(sprintf("Allocated quantities are in %s. Unused capacity and ending inventory are separate destinations.", info$settings$unit)),
        two_tables("transport_original", if (!is.null(rounded$allocation)) "transport_rounded")),
      if (show_split) shiny::tagList(
        shiny::h4("Product family allocations"),
        shiny::p(sprintf("Allocations are in %s. Annual setup estimates are separate from the aggregate plan costs.", info$settings$unit)),
        two_tables("split_original", if (show_rounded_split) "split_rounded",
          split_notes(info$value$disaggregation), if (show_rounded_split) split_notes(rounded_split)),
        if (!is.null(info$value$rounded_disaggregation_error)) {
          shiny::div(class = "alert alert-warning", info$value$rounded_disaggregation_error)
        }))
  })

  output$original_plan <- DT::renderDT({
    ap_results_datatable(plan_data(comparison()$plans, 1L))
  }, server = FALSE)
  output$rounded_plan <- DT::renderDT({
    plans <- comparison()$plans
    shiny::req(length(plans) > 1L)
    ap_results_datatable(plan_data(plans, 2L))
  }, server = FALSE)
  output$plan_costs <- DT::renderDT({
    info <- comparison()
    labels <- c(hiring = "Hiring", firing = "Firing", holding = "Inventory holding",
      regular = "Base production", overtime = "Overtime premium", idle = "Idle capacity",
      subcontract = "Subcontracting")
    data <- data.frame("Cost item" = c(unname(labels), "Total cost"), check.names = FALSE)
    for (name in names(info$plans)) {
      plan <- info$plans[[name]]
      data[[sprintf("%s (%s)", name, info$settings$currency)]] <-
        c(colSums(plan$costs_by_period[names(labels)]), plan$total_cost)
    }
    ap_results_datatable(data, cost = TRUE)
  }, server = FALSE)
  output$plan_plot <- shiny::renderPlot({
    info <- comparison()
    ap_results_plot(info$plans, info$settings$unit)
  }, alt = "Cumulative in-house production, net demand and total supply where subcontracting is used.")
  output$transport_original <- DT::renderDT({
    info <- comparison()
    allocation <- info$plans[[1L]]$allocation
    shiny::req(!is.null(allocation))
    ap_results_datatable(transport_data(allocation, info$settings$unit))
  }, server = FALSE)
  output$transport_rounded <- DT::renderDT({
    info <- comparison()
    shiny::req(length(info$plans) > 1L)
    allocation <- info$plans[[2L]]$allocation
    shiny::req(!is.null(allocation))
    ap_results_datatable(transport_data(allocation, info$settings$unit))
  }, server = FALSE)
  output$split_original <- DT::renderDT({
    info <- comparison()
    split <- info$value$disaggregation
    shiny::req(!is.null(split))
    ap_results_datatable(split_data(split, info$plans[[1L]], info$settings$currency))
  }, server = FALSE)
  output$split_rounded <- DT::renderDT({
    info <- comparison()
    split <- info$value[["rounded_disaggregation"]]
    shiny::req(length(info$plans) > 1L, !is.null(split))
    ap_results_datatable(split_data(split, info$plans[[2L]], info$settings$currency))
  }, server = FALSE)
}
