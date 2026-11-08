ap_linear_plan <- function(settings, allow_overtime = FALSE, allow_idle = FALSE,
                           allow_subcontract = FALSE, overtime_limit = 0, subcontract_limit = 0,
                           idle_cost = 0, workforce = NULL, change_periods = NULL, overtime_ratio = 1) {
  periods <- settings$periods
  valid_amounts <- function(x) is.numeric(x) && is.null(dim(x)) && all(is.finite(x) & x >= 0)
  if (!is.data.frame(periods) || !nrow(periods) || !all(c("period", "demand", "days") %in% names(periods)) ||
      !valid_amounts(periods$demand) || !valid_amounts(periods$days) || any(periods$days != floor(periods$days))) {
    pp_stop("The linear plan needs valid demand and whole working days for each period.")
  }
  n <- nrow(periods)
  if (!all(vapply(list(allow_overtime, allow_idle, allow_subcontract), function(value) {
    isTRUE(value) || isFALSE(value)
  }, logical(1)))) pp_stop("Overtime, idle time and subcontracting options must be enabled or disabled.")
  capacity_per_worker <- pp_number(settings$k, "Productivity K", lower = 0) * periods$days
  if (settings$k <= 0 || any(!is.finite(capacity_per_worker))) {
    pp_stop("Productivity K must be positive and produce finite regular capacity for each period.")
  }
  starting_workforce <- pp_number(settings$starting_workforce, "Starting workforce", lower = 0)
  starting_inventory <- pp_number(settings$starting_inventory, "Starting inventory", lower = 0)
  ending_inventory <- pp_number(settings$ending_inventory, "Ending inventory", lower = 0)
  if (length(settings$ending_mode) != 1L || !settings$ending_mode %in% c("minimum", "exact")) {
    pp_stop("Select a minimum or exact ending inventory rule.")
  }
  cost_names <- c("hiring", "firing", "holding", "regular", "overtime", "subcontract")
  if (!all(cost_names %in% names(settings$costs))) pp_stop("Enter all linear plan cost rates.")
  costs <- vapply(cost_names, function(name) {
    pp_number(settings$costs[[name]], paste("Cost of", name), lower = 0)
  }, numeric(1))
  idle_rate <- if (allow_idle) pp_number(idle_cost, "Idle time cost", lower = 0) else 0
  capacity_limit <- function(limit, enabled, name) {
    if (!enabled) return(numeric(n))
    if (!valid_amounts(limit) || !length(limit) %in% c(1L, n)) {
      pp_stop(sprintf("Enter a finite, non-negative %s limit for all periods or one for each period.", name))
    }
    rep_len(limit, n)
  }
  overtime_cap <- capacity_limit(overtime_limit, allow_overtime, "overtime")
  overtime_cap[periods$days == 0] <- 0
  if (allow_overtime) {
    overtime_per_worker <- pp_number(overtime_ratio, "Overtime ratio", lower = 0) * capacity_per_worker
    if (any(!is.finite(overtime_per_worker))) pp_stop("The overtime ratio must produce finite capacity per worker.")
  }
  subcontract_cap <- capacity_limit(subcontract_limit, allow_subcontract, "subcontracting")
  if (!is.null(workforce) && (!valid_amounts(workforce) || length(workforce) != n)) {
    pp_stop("A fixed workforce must contain one finite, non-negative value for each period.")
  }
  if (!is.null(change_periods) && (!is.numeric(change_periods) || !is.null(dim(change_periods)) ||
      any(!is.finite(change_periods) | change_periods != floor(change_periods) |
          change_periods < 1 | change_periods > n))) {
    pp_stop("Workforce changes must refer to valid period numbers.")
  }
  can_change <- if (is.null(change_periods)) rep(TRUE, n) else seq_len(n) %in% change_periods

  rates <- c(workforce = 0, hired = costs[["hiring"]], fired = costs[["firing"]],
             regular = costs[["regular"]], overtime = costs[["regular"]] + costs[["overtime"]],
             subcontract = costs[["subcontract"]], idle = idle_rate, inventory = costs[["holding"]])
  if (any(!is.finite(rates))) pp_stop("The combined regular time and overtime cost must be finite.")
  variable_names <- names(rates)
  variables <- matrix(seq_len(n * length(rates)), nrow = n, dimnames = list(NULL, variable_names))
  constraints <- list()
  add_constraint <- function(columns, coefficients, rhs, direction = "=") {
    constraints[[length(constraints) + 1L]] <<- list(columns = columns, coefficients = coefficients,
                                                   rhs = rhs, direction = direction)
  }
  for (period in seq_len(n)) {
    current <- variables[period, ]
    prior <- variables[period - 1L, , drop = FALSE]
    previous_coefficient <- rep(-1, nrow(prior))
    add_constraint(c(current[c("workforce", "hired", "fired")], prior[, "workforce"]),
                   c(1, -1, 1, previous_coefficient), if (period == 1L) starting_workforce else 0)
    add_constraint(current[c("regular", "idle", "workforce")], c(1, 1, -capacity_per_worker[period]), 0)
    add_constraint(c(current[c("inventory", "regular", "overtime", "subcontract")], prior[, "inventory"]),
                   c(1, -1, -1, -1, previous_coefficient),
                   (if (period == 1L) starting_inventory else 0) - periods$demand[period])
    add_constraint(current[["overtime"]], 1, overtime_cap[period], "<=")
    if (allow_overtime) {
      add_constraint(current[c("overtime", "workforce")], c(1, -overtime_per_worker[period]), 0, "<=")
    }
    add_constraint(current[["subcontract"]], 1, subcontract_cap[period], "<=")
    if (!allow_idle) add_constraint(current[["idle"]], 1, 0)
    if (!can_change[period]) add_constraint(current[c("hired", "fired")], c(1, 1), 0)
    if (!is.null(workforce)) add_constraint(current[["workforce"]], 1, workforce[period])
  }
  add_constraint(variables[n, "inventory"], 1, ending_inventory,
                 if (settings$ending_mode == "exact") "=" else ">=")

  # Build lpSolve's coordinate matrix without allocating a period-by-period dense matrix.
  terms <- do.call(rbind, lapply(seq_along(constraints), function(index) {
    constraint <- constraints[[index]]
    nonzero <- constraint$coefficients != 0
    cbind(index, constraint$columns[nonzero], constraint$coefficients[nonzero])
  }))
  rhs <- vapply(constraints, function(constraint) constraint$rhs, numeric(1))
  directions <- vapply(constraints, function(constraint) constraint$direction, character(1))
  if (!requireNamespace("lpSolve", quietly = TRUE)) pp_stop("The lpSolve package is required for the linear plan.")
  result <- lpSolve::lp("min", objective.in = rep(rates, each = n), const.dir = directions,
                       const.rhs = rhs, dense.const = terms)
  if (result$status == 2L) {
    pp_stop("No feasible linear plan meets demand and the ending inventory rule with these capacity and workforce settings.")
  }
  if (result$status != 0L) pp_stop(sprintf("The linear solver did not return an optimal plan (status %d).", result$status))
  if (any(!is.finite(result$solution) | result$solution < -1e-7)) {
    pp_stop("The linear solver returned invalid plan quantities.")
  }

  plan <- matrix(pmax(result$solution, 0), nrow = n, dimnames = list(NULL, variable_names))
  if (!is.null(workforce)) plan[, "workforce"] <- workforce
  change <- diff(c(starting_workforce, plan[, "workforce"]))
  plan[, "hired"] <- pmax(change, 0)
  plan[, "fired"] <- pmax(-change, 0)
  # Shift any simultaneous overtime and idle units into regular production.
  overlap <- pmin(plan[, "overtime"], plan[, "idle"])
  plan[, "regular"] <- plan[, "regular"] + overlap
  plan[, "overtime"] <- plan[, "overtime"] - overlap
  plan[, "idle"] <- plan[, "idle"] - overlap

  contributions <- terms[, 3L] * plan[terms[, 2L]]
  lhs <- rowsum(contributions, terms[, 1L], reorder = FALSE)[, 1L]
  tolerance <- 1e-7 + 1e-9 * pmax(abs(rhs), rowsum(abs(contributions), terms[, 1L], reorder = FALSE)[, 1L])
  residual <- lhs - rhs
  if (any(!is.finite(residual) |
          (directions == "=" & abs(residual) > tolerance) |
          (directions == "<=" & residual > tolerance) |
          (directions == ">=" & residual < -tolerance))) {
    pp_stop("The linear solver result could not be reconciled with the planning constraints.")
  }
  components <- c("workforce", "regular", "overtime", "subcontract", "idle")
  setNames(lapply(components, function(name) unname(plan[, name])), components)
}
