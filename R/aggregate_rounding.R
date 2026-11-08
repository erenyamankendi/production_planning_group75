ap_round_plan <- function(request, plan) {
  settings <- request$settings
  workforce <- plan$rows$workforce
  nearest <- round(workforce)
  already_whole <- workforce == nearest | (nearest > 0 & abs(workforce - nearest) <= 1e-8)
  if (all(already_whole)) {
    return(list(plan = plan, status = "unchanged", message = "The plan already uses whole workers."))
  }
  rounded <- ceiling(workforce)
  rounded[already_whole] <- nearest[already_whole]

  method <- request$method
  idle_cost <- if (identical(method, "lp") && isTRUE(request$allow_idle)) request$idle_cost else 0
  solve <- function(selected_settings) {
    if (method %in% c("chase", "level")) {
      raw <- ap_regular_plan(rounded, selected_settings$k * selected_settings$periods$days)
    } else if (method %in% c("mixed", "lp")) {
      raw <- ap_review_linear_plan(request, settings = selected_settings, workforce = rounded)
    } else {
      pp_stop("The transportation plan requires a whole starting workforce for this comparison.")
    }
    ap_check_plan(selected_settings, raw, idle_cost = idle_cost)
  }

  checked <- tryCatch(solve(settings), error = identity)
  if (!inherits(checked, "error")) {
    return(list(plan = checked, status = "feasible", message = "The rounded workforce plan meets the selected settings."))
  }
  if (identical(settings$ending_mode, "exact")) {
    minimum <- settings
    minimum$ending_mode <- "minimum"
    candidate <- tryCatch(solve(minimum), error = identity)
    if (!inherits(candidate, "error")) {
      return(list(plan = NULL, status = "infeasible", candidate = candidate,
        message = sprintf(paste("The rounded workforce cannot meet the exact ending inventory target of %.8g.",
                                "The separate Minimum candidate ends with %.8g."),
                          settings$ending_inventory, candidate$ending_inventory)))
    }
  }
  list(plan = NULL, status = "infeasible",
       message = paste("The rounded workforce comparison is unavailable.", conditionMessage(checked)))
}
