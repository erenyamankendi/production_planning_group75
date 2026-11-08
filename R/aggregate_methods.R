ap_regular_plan <- function(workforce, capacity_per_worker) {
  n <- length(capacity_per_worker)
  zero <- numeric(n)
  list(workforce = workforce, regular = workforce * capacity_per_worker,
       overtime = zero, subcontract = zero, idle = zero)
}

ap_chase_plan <- function(settings) {
  periods <- settings$periods
  capacity <- settings$k * periods$days
  required <- periods$demand
  n <- nrow(periods)
  required[n] <- required[n] + settings$ending_inventory
  # If a plant is closed, its demand must be covered by earlier production or stock.
  carry <- 0
  for (i in rev(seq_len(n))) {
    if (capacity[i] == 0) {
      carry <- carry + required[i]
    } else {
      required[i] <- required[i] + carry
      carry <- 0
    }
  }
  if (carry > settings$starting_inventory) pp_stop("Opening inventory cannot cover demand before the first working period.")
  inventory <- settings$starting_inventory
  workforce <- numeric(n)
  current <- settings$starting_workforce
  for (i in seq_len(n)) {
    if (capacity[i] > 0) current <- max(0, required[i] - inventory) / capacity[i]
    workforce[i] <- current
    inventory <- inventory + current * capacity[i] - periods$demand[i]
  }
  ap_regular_plan(workforce, capacity)
}

ap_level_plan <- function(settings) {
  periods <- settings$periods
  capacity <- settings$k * periods$days
  cumulative_capacity <- cumsum(capacity)
  required <- cumsum(periods$demand) - settings$starting_inventory
  last <- nrow(periods)
  required[last] <- required[last] + settings$ending_inventory
  working <- cumulative_capacity > 0
  if (any(required[!working] > 0)) pp_stop("Opening inventory cannot cover demand before the first working period.")
  # Every demand prefix must be covered by one constant headcount.
  workforce <- max(0, required[working] / cumulative_capacity[working])
  ap_regular_plan(rep(workforce, last), capacity)
}

ap_check_plan <- function(settings, raw, idle_cost = 0) {
  periods <- settings$periods
  fields <- c("workforce", "regular", "overtime", "subcontract", "idle")
  valid <- vapply(raw[fields], function(x) {
    is.numeric(x) && length(x) == nrow(periods) && all(is.finite(x) & x >= 0)
  }, logical(1))
  if (!all(valid)) pp_stop("The method returned an invalid plan quantity.")
  production <- raw$regular + raw$overtime
  supply <- production + raw$subcontract
  capacity <- raw$workforce * settings$k * periods$days
  if (any(!is.finite(capacity)) || any(abs(raw$regular + raw$idle - capacity) > 1e-7 * pmax(1, capacity))) {
    pp_stop("The plan does not balance regular production and idle capacity.")
  }
  cumulative_supply <- cumsum(supply)
  cumulative_demand <- cumsum(periods$demand)
  net_demand <- cumulative_demand - settings$starting_inventory
  last <- nrow(periods)
  net_demand[last] <- net_demand[last] + settings$ending_inventory
  inventory <- settings$starting_inventory + cumulative_supply - cumulative_demand
  stock_tolerance <- 1e-7 * pmax(1, settings$starting_inventory + cumulative_supply, cumulative_demand)
  if (any(!is.finite(inventory))) pp_stop("The plan exceeds the numeric range.")
  if (any(inventory < -stock_tolerance)) {
    period <- periods$period[which(inventory < -stock_tolerance)[1L]]
    pp_stop(sprintf("This plan cannot meet demand in %s with the selected constraints.", period))
  }
  inventory <- pmax(0, inventory)
  target_gap <- inventory[last] - settings$ending_inventory
  target_tolerance <- max(stock_tolerance[last], 1e-7 * settings$ending_inventory)
  if (target_gap < -target_tolerance) pp_stop("This plan cannot reach the minimum ending inventory.")
  if (settings$ending_mode == "exact" && abs(target_gap) > target_tolerance) {
    pp_stop(sprintf("This plan ends with %.8g units and cannot meet the exact target of %.8g units.",
                    inventory[last], settings$ending_inventory))
  }
  workforce_path <- c(settings$starting_workforce, raw$workforce)
  changes <- diff(workforce_path)
  adjacent_scale <- pmax(head(workforce_path, -1L), tail(workforce_path, -1L))
  changes[abs(changes) <= 8 * .Machine$double.eps * adjacent_scale] <- 0
  hired <- pmax(0, changes)
  fired <- pmax(0, -changes)
  costs <- settings$costs
  costs_by_period <- data.frame(hiring = hired * costs[["hiring"]], firing = fired * costs[["firing"]],
    holding = inventory * costs[["holding"]], regular = production * costs[["regular"]],
    overtime = raw$overtime * costs[["overtime"]], idle = raw$idle * idle_cost,
    subcontract = raw$subcontract * costs[["subcontract"]])
  total <- sum(as.matrix(costs_by_period))
  if (!is.finite(total)) pp_stop("The plan cost exceeds the numeric range.")
  allocation_cost <- raw$details$allocation_cost
  if (!is.null(allocation_cost)) {
    supply_cost <- sum(as.matrix(costs_by_period[c("holding", "regular", "overtime", "subcontract")]))
    if (abs(supply_cost - allocation_cost) > 1e-7 * max(1, supply_cost)) {
      pp_stop("The transportation allocation cost does not reconcile with the period plan.")
    }
  }
  rows <- data.frame(period = periods$period, workforce = raw$workforce, hired = hired, fired = fired,
    demand = periods$demand, production = production, regular = raw$regular, overtime = raw$overtime,
    subcontract = raw$subcontract, idle = raw$idle, inventory = inventory,
    cumulative_supply = cumulative_supply, cumulative_net_demand = net_demand)
  list(rows = rows, costs_by_period = costs_by_period, total_cost = total,
       ending_inventory = inventory[last], allocation = raw$allocation, details = raw$details)
}
