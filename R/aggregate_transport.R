ap_transport_plan <- function(settings, overtime_limit = 0, subcontract_limit = 0,
                              workforce = settings$starting_workforce,
                              method = c("northwest", "least_cost", "optimal")) {
  method <- match.arg(method)
  periods <- settings$periods
  n <- nrow(periods)
  if (!is.data.frame(periods) || n < 1L || !all(c("period", "demand", "days") %in% names(periods))) {
    pp_stop("Transportation planning needs at least one period with demand and working days.")
  }
  amount <- function(value, label, lengths) {
    if (!is.numeric(value) || !is.null(dim(value)) || !length(value) %in% lengths ||
        any(!is.finite(value) | value < 0)) {
      pp_stop(sprintf("%s must contain finite non-negative amounts for the planning periods.", label))
    }
    as.numeric(value)
  }
  labels <- as.character(periods$period)
  demand <- amount(periods$demand, "Demand", n)
  days <- amount(periods$days, "Working days", n)
  workforce <- rep_len(amount(workforce, "Workforce", c(1L, n)), n)
  k <- amount(settings$k, "Productivity", 1L)
  if (k == 0) pp_stop("Productivity must be positive.")
  opening <- amount(settings$starting_inventory, "Starting inventory", 1L)
  target <- amount(settings$ending_inventory, "Ending inventory target", 1L)
  if (length(settings$ending_mode) != 1L || !settings$ending_mode %in% c("minimum", "exact")) {
    pp_stop("Choose a minimum or exact ending inventory rule.")
  }
  exact <- settings$ending_mode == "exact"
  costs <- settings$costs[c("regular", "overtime", "subcontract", "holding")]
  cost_values <- amount(costs, "Transportation costs", 4L)
  names(cost_values) <- names(costs)
  overtime_limit <- rep_len(amount(overtime_limit, "Overtime capacity", c(1L, n)), n)
  overtime_limit[days == 0 | workforce == 0] <- 0
  subcontract_limit <- rep_len(amount(subcontract_limit, "Subcontract capacity", c(1L, n)), n)
  capacity <- rbind(workforce * k * days, overtime_limit, subcontract_limit)
  supply <- c(opening, as.numeric(capacity))
  required <- c(demand, target)
  if (!is.finite(sum(supply)) || !is.finite(sum(required))) {
    pp_stop("The total transportation supply or requirement exceeds the numeric range.")
  }

  tolerance <- function(x) sqrt(.Machine$double.eps) * pmax(abs(x), .Machine$double.eps)
  cumulative_demand <- cumsum(demand)
  cumulative_supply <- opening + cumsum(colSums(capacity))
  short <- which(cumulative_demand - cumulative_supply > tolerance(cumulative_demand))
  if (length(short)) {
    pp_stop(sprintf("Available capacity and starting inventory cannot cover demand through %s.", labels[short[1L]]))
  }
  if (sum(required) - sum(supply) > tolerance(sum(required))) {
    pp_stop("Available capacity and starting inventory cannot meet the ending inventory target.")
  }
  if (exact && opening - sum(required) > tolerance(opening)) {
    pp_stop("Starting inventory already exceeds total demand plus the exact ending inventory target.")
  }

  end_col <- n + 1L
  source_period <- c(0L, rep(seq_len(n), each = 3L))
  source_modes <- c("Regular", "Overtime", "Subcontract")
  source_labels <- c("Opening inventory", paste(rep(source_modes, n), rep(labels, each = 3L), sep = " - "))
  destination_labels <- make.unique(c(labels, "Ending inventory", "Unused capacity"))
  production_cost <- c(0, rep(c(cost_values["regular"], sum(cost_values[c("regular", "overtime")]),
                                cost_values["subcontract"]), n))
  holding_periods <- outer(pmax(1L, source_period), seq_len(end_col), function(source, destination) destination - source)
  unit_cost <- sweep(pmax(holding_periods, 0) * cost_values["holding"], 1L, production_cost, "+")
  if (any(!is.finite(unit_cost))) pp_stop("Transportation unit costs exceed the numeric range.")
  unit_cost[outer(source_period, seq_len(end_col), `>`)] <- NA_real_
  allocation <- matrix(0, nrow = length(supply), ncol = end_col,
                       dimnames = list(source_labels, destination_labels[seq_len(end_col)]))
  active_destinations <- required > 0
  if (method == "optimal" && !exact) active_destinations[end_col] <- TRUE
  cells <- which(!is.na(unit_cost) & supply > 0 &
                   rep(active_destinations, each = length(supply)), arr.ind = TRUE)
  objective <- NULL

  if (method == "optimal" && nrow(cells)) {
    if (!requireNamespace("lpSolve", quietly = TRUE)) {
      pp_stop("Install the lpSolve package to run optimal transportation planning.")
    }
    # One variable per allowed delivery arc. Capacity rows are upper bounds.
    # The opening stock row must account for every unit already on hand.
    active_rows <- unique(cells[, 1L])
    active_cols <- unique(cells[, 2L])
    variable <- seq_len(nrow(cells))
    constraints <- rbind(cbind(match(cells[, 1L], active_rows), variable, 1),
                         cbind(length(active_rows) + match(cells[, 2L], active_cols), variable, 1))
    fit <- lpSolve::lp("min", unit_cost[cells],
                       const.dir = c(ifelse(active_rows == 1L, "=", "<="),
                                     ifelse(active_cols == end_col & !exact, ">=", "=")),
                       const.rhs = c(supply[active_rows], required[active_cols]),
                       dense.const = constraints)
    if (fit$status != 0L) {
      pp_stop(sprintf("The transportation solver did not return a verified minimum (status %s).", fit$status))
    }
    if (any(!is.finite(fit$solution) | fit$solution < -sqrt(.Machine$double.eps))) {
      pp_stop("The transportation solver returned invalid allocation quantities.")
    }
    allocation[cells] <- pmax(fit$solution, 0)
    objective <- fit$objval
  } else if (method != "optimal") {
    # Column-major order visits demand periods first, then available sources in order.
    # The cheapest-cell rule ranks the same physical arcs by cost, with that order as its tie break.
    if (method == "least_cost" && nrow(cells)) {
      cells <- cells[order(unit_cost[cells], cells[, 2L], cells[, 1L]), , drop = FALSE]
    }
    available <- supply
    unfilled <- required
    production_room <- if (exact) max(0, sum(required) - opening) else Inf
    for (cell in seq_len(nrow(cells))) {
      row <- cells[cell, 1L]
      col <- cells[cell, 2L]
      quantity <- min(available[row], unfilled[col], if (row == 1L) Inf else production_room)
      allocation[row, col] <- quantity
      available[row] <- available[row] - quantity
      unfilled[col] <- unfilled[col] - quantity
      if (row != 1L) production_room <- production_room - quantity
    }
    blocked <- which(unfilled > tolerance(required))
    if (length(blocked)) {
      pp_stop(sprintf("The selected transportation heuristic could not fill %s. Try the optimal method.",
                      destination_labels[blocked[1L]]))
    }
    if (!exact) allocation[1L, end_col] <- allocation[1L, end_col] + available[1L]
  }

  used <- rowSums(allocation)
  delivered <- colSums(allocation)
  if (any(abs(delivered[seq_len(n)] - demand) > tolerance(demand)) ||
      abs(used[1L] - opening) > tolerance(opening) ||
      any(used[-1L] - supply[-1L] > tolerance(supply[-1L])) ||
      target - delivered[end_col] > tolerance(target) ||
      (exact && delivered[end_col] - target > tolerance(target))) {
    pp_stop("The transportation allocation did not pass the demand, capacity and ending stock checks.")
  }
  allocation_cost <- sum(allocation * unit_cost, na.rm = TRUE)
  if (!is.finite(allocation_cost) ||
      (!is.null(objective) && (!is.finite(objective) ||
                              abs(allocation_cost - objective) > tolerance(allocation_cost)))) {
    pp_stop("The transportation allocation did not pass the objective cost check.")
  }

  unused <- c(0, pmax(supply[-1L] - used[-1L], 0))
  allocation <- cbind(allocation, unused)
  colnames(allocation) <- destination_labels
  unit_cost <- cbind(unit_cost, c(NA_real_, rep(0, length(supply) - 1L)))
  dimnames(unit_cost) <- dimnames(allocation)
  regular_rows <- seq.int(2L, by = 3L, length.out = n)
  list(workforce = workforce, regular = used[regular_rows], overtime = used[regular_rows + 1L],
       subcontract = used[regular_rows + 2L], idle = unused[regular_rows], allocation = allocation,
       details = list(method = method, unit_cost = unit_cost, supply = stats::setNames(supply, source_labels),
                      demand = stats::setNames(required, destination_labels[seq_len(end_col)]),
                      ending_inventory = unname(delivered[end_col]), allocation_cost = allocation_cost))
}
