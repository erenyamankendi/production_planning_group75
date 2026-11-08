# Aggregate Planning method references and Shiny calculation wiring.
local({
  arg <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
  app <- dirname(dirname(normalizePath(arg)))
  version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
  .libPaths(c(file.path(dirname(app), ".R-library"),
    file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", version), .libPaths()))
  for (path in list.files(file.path(app, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(path, envir = environment())

  close_to <- function(actual, expected, tolerance = 1e-8) {
    stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance)))
  }
  fails <- function(expression, message) {
    error <- tryCatch({ force(expression); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(message, conditionMessage(error), fixed = TRUE))
  }
  settings <- function(demand, days = rep(1, length(demand)), workforce = 1, opening = 0,
                       ending = 0, rule = "minimum", k = 1, costs = numeric(6L)) {
    list(periods = data.frame(period = paste("P", seq_along(demand)), demand = demand, days = days),
      starting_workforce = workforce, starting_inventory = opening, ending_inventory = ending,
      ending_mode = rule, k = k, costs = setNames(costs, names(ap_cost_labels)), unit = "units", currency = "USD")
  }

  basic <- settings(c(30, 10, 20), k = 10)
  chase <- ap_check_plan(basic, ap_chase_plan(basic))
  close_to(chase$rows$workforce, c(3, 1, 2))
  close_to(chase$rows$inventory, c(0, 0, 0))
  level <- ap_check_plan(basic, ap_level_plan(basic))
  close_to(level$rows$workforce, c(3, 3, 3))
  close_to(level$rows$inventory, c(0, 20, 30))
  exact <- basic
  exact$ending_mode <- "exact"
  fails(ap_check_plan(exact, ap_level_plan(exact)), "cannot meet the exact target")
  mixed <- ap_check_plan(exact, ap_linear_plan(exact, change_periods = c(1L, 3L)))
  close_to(mixed$rows$workforce, c(3, 3, 0))
  closed <- settings(c(5, 5), c(1, 0), k = 10)
  close_to(ap_check_plan(closed, ap_chase_plan(closed))$rows$production, c(10, 0))
  cat("PASS chase, level, mixed, closed periods and exact target handling.\n")

  densepack <- settings(c(1280, 640, 900, 1200, 2000, 1400), c(20, 24, 18, 26, 22, 15),
    workforce = 300, opening = 500, ending = 600, rule = "exact", k = 245 / (76 * 22),
    costs = c(500, 1000, 80, 0, 0, 0))
  lp <- ap_check_plan(densepack, ap_linear_plan(densepack))
  close_to(lp$total_cost, 379292.22, tolerance = 1e-7)
  close_to(lp$ending_inventory, 600)
  stopifnot(lp$rows$hired[6] == 0, lp$rows$fired[6] == 0)
  close_to(lp$rows$cumulative_net_demand, c(780, 1420, 2320, 3520, 5520, 7520))
  overtime <- settings(15, rule = "exact", k = 10, costs = c(0, 0, 0, 2, 3, 4))
  overtime_raw <- ap_linear_plan(overtime, allow_overtime = TRUE, overtime_limit = 5,
    workforce = 1, overtime_ratio = 0.5)
  close_to(overtime_raw$overtime, 5)
  close_to(ap_check_plan(overtime, overtime_raw)$total_cost, 45)
  fails(ap_linear_plan(overtime, allow_overtime = TRUE, overtime_limit = 5,
    workforce = 1, overtime_ratio = 0.4), "No feasible linear plan")
  idle <- settings(4, rule = "exact", k = 10, costs = c(0, 0, 0, 2, 3, 4))
  idle_raw <- ap_linear_plan(idle, allow_idle = TRUE, idle_cost = 1, workforce = 1)
  close_to(idle_raw$idle, 6)
  close_to(ap_check_plan(idle, idle_raw, idle_cost = 1)$total_cost, 14)
  subcontract <- settings(7, days = 0)
  subcontract_raw <- ap_linear_plan(subcontract, allow_subcontract = TRUE, subcontract_limit = 7, workforce = 1)
  close_to(subcontract_raw$subcontract, 7)
  cat("PASS Densepack LP, overtime cap, idle time and subcontracting.\n")

  transport <- settings(c(1, 3), workforce = 2, opening = 1, ending = 1, costs = c(0, 0, 1, 4, 2, 9))
  transport_costs <- vapply(c("northwest", "least_cost", "optimal"), function(method) {
    ap_check_plan(transport, ap_transport_plan(transport, overtime_limit = 2,
      subcontract_limit = 2, method = method))$total_cost
  }, numeric(1))
  close_to(transport_costs, c(25, 19, 19))
  greedy_gap <- settings(c(1, 0), workforce = 2, opening = 2, ending = 2, costs = c(0, 0, 5, 1, 0, 0))
  close_to(ap_check_plan(greedy_gap, ap_transport_plan(greedy_gap, method = "least_cost"))$total_cost, 22)
  close_to(ap_check_plan(greedy_gap, ap_transport_plan(greedy_gap, method = "optimal"))$total_cost, 16)
  cat("PASS transportation starting plans, verified minimum and carrying cost reconciliation.\n")

  families <- data.frame(family = c("A", "B"), setup_cost = c(1, 4), annual_usage = c(100, 100))
  split <- ap_disaggregate(300, families)
  close_to(split$allocations, c(100, 200))
  close_to(split$annual_setup_cost, 3)
  families$maximum <- c(Inf, 180)
  close_to(ap_disaggregate(300, families)$allocations, c(120, 180))
  stopifnot(is.na(ap_disaggregate(0, families)$annual_setup_cost))
  cat("PASS square-root rule and binding family bounds.\n")

  review <- function(settings, method = "lp", idle = FALSE, idle_cost = 0, families = NULL) {
    ap_run_review(list(settings = settings, method = method, allow_overtime = FALSE, allow_idle = idle,
      allow_subcontract = FALSE, overtime_limit = "0", subcontract_limit = "0", overtime_ratio = 0,
      idle_cost = idle_cost, families = families))
  }
  densepack_exact <- review(densepack)
  stopifnot(densepack_exact$rounding$status == "infeasible", is.null(densepack_exact$rounding$plan))
  close_to(densepack_exact$plan$total_cost, 379292.222835)
  close_to(densepack_exact$rounding$candidate$rows$workforce, c(rep(273, 4), 738, 738))
  close_to(densepack_exact$rounding$candidate$ending_inventory, 601.4413875598)
  close_to(densepack_exact$rounding$candidate$total_cost, 379613.8755981)
  densepack_minimum <- densepack
  densepack_minimum$ending_mode <- "minimum"
  rounded_densepack <- review(densepack_minimum)$rounding
  stopifnot(rounded_densepack$status == "feasible")
  close_to(rounded_densepack$plan$ending_inventory, 601.4413875598)
  close_to(rounded_densepack$plan$total_cost, 379613.8755981)

  fractional <- settings(15, rule = "exact", k = 10, costs = c(5, 0, 1, 2, 0, 0))
  with_idle <- review(fractional, idle = TRUE, idle_cost = 0.5)
  close_to(with_idle$plan$total_cost, 32.5)
  stopifnot(with_idle$rounding$status == "feasible")
  close_to(with_idle$rounding$plan$rows$workforce, 2)
  close_to(with_idle$rounding$plan$rows$idle, 5)
  close_to(with_idle$rounding$plan$total_cost, 37.5)
  close_to(with_idle$rounding$plan$ending_inventory, 0)
  rounded_chase <- review(fractional, method = "chase")$rounding
  stopifnot(rounded_chase$status == "infeasible")
  close_to(rounded_chase$candidate$total_cost, 50)
  close_to(rounded_chase$candidate$ending_inventory, 5)
  stopifnot(review(basic, method = "level")$rounding$status == "unchanged")
  small_workforce <- review(settings(1, k = 1e9), method = "chase")$rounding
  stopifnot(small_workforce$status == "feasible")
  close_to(small_workforce$plan$rows$workforce, 1)

  fractional$ending_mode <- "minimum"
  family_with_cap <- data.frame(family = "A", setup_cost = 1, annual_usage = 10, maximum = 15)
  with_family <- review(fractional, method = "chase", families = family_with_cap)
  close_to(with_family$disaggregation$allocations, 15)
  stopifnot(with_family$rounding$status == "feasible", is.null(with_family$rounded_disaggregation),
    grepl("cannot accommodate", with_family$rounded_disaggregation_error, fixed = TRUE))
  cat("PASS rounded Densepack, exact and minimum targets, idle time and family allocation limits.\n")

  chart_inputs <- settings(c(8, 22, 11), workforce = 1, opening = 4, ending = 2, rule = "exact",
    k = 10, costs = c(5, 3, 1, 2, 1, 6))
  chart_plan <- ap_check_plan(chart_inputs, list(workforce = c(1, 2, 1), regular = c(6, 20, 8),
    overtime = c(0, 2, 0), subcontract = c(0, 0, 3), idle = c(4, 0, 2)), idle_cost = 0.5)
  close_to(cumsum(chart_plan$rows$production), c(6, 28, 36))
  close_to(chart_plan$rows$cumulative_supply, c(6, 28, 39))
  close_to(chart_plan$rows$cumulative_net_demand, c(4, 26, 39))
  close_to(colSums(chart_plan$costs_by_period), c(5, 3, 6, 72, 2, 3, 18))
  close_to(chart_plan$total_cost, 109)
  cat("PASS cumulative supply, net demand, inventory and seven cost items reconcile.\n")

  html <- htmltools::renderTags(aggregate_planning_ui("aggregate"))$html
  stopifnot(grepl("aggregate-methods-run_plan", html, fixed = TRUE))
  shiny::testServer(ap_method_review_server, args = list(
    data_bundle = shiny::reactive(basic), period_data = shiny::reactive(basic$periods)), {
    session$setInputs(method = "chase", run_plan = 0)
    session$setInputs(run_plan = 1)
    close_to(current()$value$plan$rows$workforce, c(3, 1, 2))
    session$setInputs(method = "level")
    stopifnot(identical(current()$message, "Inputs changed. Run the plan again."))
    session$setInputs(run_plan = 2)
    close_to(current()$value$plan$rows$workforce, c(3, 3, 3))
    session$setInputs(method = "disaggregation", source_method = "chase")
    for (row in 1:2) {
      session$setInputs(families_cell_edit = list(row = row, col = 1L, value = as.character(row^2)))
      session$setInputs(families_cell_edit = list(row = row, col = 2L, value = "100"))
    }
    session$setInputs(run_plan = 3)
    close_to(current()$value$disaggregation$allocations[1, ], c(10, 20))
  })
  cat("PASS Shiny method preview, edited family inputs and stale result handling.\n")
})
