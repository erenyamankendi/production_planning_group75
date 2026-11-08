ap_disaggregate <- function(production, families) {
  if (!is.numeric(production) || !is.null(dim(production)) || !length(production) ||
      any(!is.finite(production) | production < 0)) {
    pp_stop("Enter finite non-negative aggregate production for each period.")
  }
  required <- c("family", "setup_cost", "annual_usage")
  if (!is.data.frame(families) || !nrow(families) || anyDuplicated(names(families)) ||
      !all(required %in% names(families))) {
    pp_stop("Enter product families with a name, setup cost and annual usage for each family.")
  }
  labels <- trimws(as.character(families$family))
  if (length(labels) != nrow(families) || anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(tolower(labels))) {
    pp_stop("Product family names must be non-empty and unique.")
  }
  for (field in c("setup_cost", "annual_usage")) {
    values <- families[[field]]
    if (!is.numeric(values) || !is.null(dim(values)) || any(!is.finite(values) | values < 0)) {
      pp_stop("Family setup costs and annual usage must be finite non-negative numbers.")
    }
  }
  read_bound <- function(field, default) {
    values <- families[[field]]
    if (is.null(values)) return(rep(default, nrow(families)))
    if (is.logical(values) && all(is.na(values))) values <- as.numeric(values)
    if (!is.numeric(values) || !is.null(dim(values)) || any(is.nan(values))) {
      pp_stop("Family production bounds must be numeric or blank.")
    }
    values[is.na(values)] <- default
    values
  }
  minimum <- read_bound("minimum", 0)
  maximum <- read_bound("maximum", Inf)
  if (any(!is.finite(minimum) | minimum < 0 | maximum < minimum)) {
    pp_stop("Family minimums must be finite and non-negative, and each maximum must be at least its minimum.")
  }

  positive <- families$setup_cost > 0 & families$annual_usage > 0
  log_setup_usage <- log(families$setup_cost[positive]) + log(families$annual_usage[positive])
  log_weights <- log_setup_usage / 2
  if (length(log_weights)) log_weights <- log_weights - max(log_weights)
  positive_maximum <- sum(maximum[positive])
  minimum_total <- sum(minimum)
  maximum_total <- sum(maximum)
  allocations <- matrix(0, nrow = length(production), ncol = length(labels),
                        dimnames = list(names(production), labels))
  annual_setup_cost <- stats::setNames(numeric(length(production)), names(production))
  notes <- stats::setNames(rep("", length(production)), names(production))

  allocate_positive <- function(amount) {
    lower <- minimum[positive]
    upper <- maximum[positive]
    slack <- amount - sum(lower)
    if (slack <= 1e-12 * amount) return(lower)
    if (positive_maximum - amount <= 1e-12 * amount) return(upper)

    quantity <- function(log_share) {
      allocation <- amount * exp(log_share)
      tiny <- log_share < log(.Machine$double.xmin)
      allocation[tiny] <- exp(log(amount) + log_share[tiny])
      allocation
    }
    direct <- quantity(log_weights - log(sum(exp(log_weights))))
    if (all(direct >= lower & direct <= upper)) return(direct)

    # With bounds, one multiplier scales the square-root weights until the allocation balances.
    lower_share <- lower / amount
    upper_share <- pmin(2, upper / amount)
    share <- function(level) pmin(upper_share, pmax(lower_share, exp(pmin(log(2), log_weights + level))))
    low <- log(slack / amount) - log(length(log_weights)) - 1
    high <- log(2) - min(log_weights)
    level <- stats::uniroot(function(level) sum(share(level)) - 1, c(low, high), tol = 1e-12)$root
    pmin(upper, pmax(lower, quantity(pmin(0, log_weights + level))))
  }

  for (period in seq_along(production)) {
    total <- production[period]
    tolerance <- 1e-10 * total
    period_label <- if (is.null(names(production))) period else names(production)[period]
    if (minimum_total - total > tolerance || total - maximum_total > tolerance) {
      pp_stop(sprintf("Family production bounds cannot accommodate aggregate production in period %s.", period_label))
    }
    assigned <- minimum
    available <- total - sum(minimum[!positive])
    if (any(positive)) assigned[positive] <- allocate_positive(min(available, positive_maximum))

    # Zero-weight families have no setup objective. Allocate any remaining volume in table order.
    remaining <- total - sum(assigned)
    for (family in which(!positive)) {
      if (remaining <= tolerance) break
      quantity <- min(remaining, maximum[family] - assigned[family])
      assigned[family] <- assigned[family] + quantity
      remaining <- remaining - quantity
    }
    gap <- total - sum(assigned)
    if (abs(gap) > tolerance) {
      pp_stop(sprintf("Family production could not be balanced in period %s.", period_label))
    }
    if (gap != 0) {
      # Remove small numerical residuals without crossing a family bound.
      room <- if (gap > 0) maximum - assigned else assigned - minimum
      candidates <- which(room >= abs(gap))
      family <- candidates[which.max(assigned[candidates])]
      assigned[family] <- assigned[family] + gap
    }
    allocations[period, ] <- assigned

    if (any(assigned[positive] == 0)) {
      annual_setup_cost[period] <- NA_real_
      notes[period] <- "Annual setup cost is undefined for a family with positive setup cost and usage but zero production."
    } else {
      cost <- sum(exp(log_setup_usage - log(assigned[positive])))
      annual_setup_cost[period] <- if (is.finite(cost)) cost else NA_real_
      if (!is.finite(cost)) notes[period] <- "Annual setup cost exceeds the numeric range."
    }
  }

  list(allocations = allocations, annual_setup_cost = annual_setup_cost, notes = notes)
}
