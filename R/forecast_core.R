# Calculation functions are independent of Shiny sessions and file uploads.
# Forecast errors consistently mean actual minus forecast.

pp_stop <- function(message) stop(message, call. = FALSE)

pp_integer <- function(x, label, lower = 1L, upper = Inf) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x != floor(x) ||
      x < lower || x > upper) {
    pp_stop(sprintf("%s must be an integer between %s and %s.", label, lower, upper))
  }
  as.integer(x)
}

pp_number <- function(x, label, lower = -Inf, upper = Inf) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < lower || x > upper) {
    pp_stop(sprintf("%s must be a finite number between %s and %s.", label, lower, upper))
  }
  as.numeric(x)
}

pp_series <- function(y) {
  if (!is.numeric(y) || inherits(y, c("Date", "POSIXt")) || !length(y)) {
    pp_stop("Select a non-empty numeric target series.")
  }
  bad <- which(!is.finite(y))
  if (length(bad)) pp_stop(sprintf("Missing or non-finite target values at rows: %s. Correct the data before forecasting.",
                                  paste(head(bad, 12L), collapse = ", ")))
  as.numeric(y)
}

pp_metrics <- function(actual, fitted, evaluation = seq_along(actual)) {
  evaluation <- evaluation[evaluation >= 1L & evaluation <= length(actual)]
  idx <- evaluation[is.finite(actual[evaluation]) & is.finite(fitted[evaluation])]
  if (!length(idx)) return(list(mad = NA_real_, mse = NA_real_, mape = NA_real_,
                                count = 0L, periods = integer(), note = "No historical forecast errors are available."))
  errors <- actual[idx] - fitted[idx]
  has_zero <- any(actual[idx] == 0)
  list(mad = mean(abs(errors)), mse = mean(errors^2),
       mape = if (has_zero) NA_real_ else 100 * mean(abs(errors / actual[idx])),
       count = length(idx), periods = idx,
       note = if (has_zero) "MAPE is undefined because the evaluation period includes a zero actual value." else "")
}

pp_result <- function(method, y, fitted, next_forecast, parameters = list(),
                      evaluation = seq_along(y), accuracy_label = "Historical one-step accuracy",
                      details = list(), warnings = character()) {
  if (length(fitted) != length(y) || length(next_forecast) != 1L || !is.finite(next_forecast)) {
    pp_stop("The selected model could not produce a finite next-period forecast.")
  }
  metrics <- pp_metrics(y, fitted, evaluation)
  if (all(y >= 0) && next_forecast < 0) warnings <- c(warnings,
    "The model produced a negative forecast for a non-negative series. The forecast has not been clipped; review the method and settings.")
  list(method = method, actual = y, fitted = as.numeric(fitted),
       forecast = as.numeric(next_forecast), errors = y - fitted,
       metrics = metrics, parameters = parameters, accuracy_label = accuracy_label,
       details = details, warnings = unique(c(warnings, metrics$note[nzchar(metrics$note)])))
}

pp_ma_fitted <- function(y, n) {
  fitted <- rep(NA_real_, length(y))
  if (length(y) > n) {
    for (t in seq.int(n + 1L, length(y))) fitted[t] <- mean(y[seq.int(t - n, t - 1L)])
  }
  fitted
}

pp_moving_average <- function(y, n = 3L, mode = "manual", search_max = NULL) {
  y <- pp_series(y)
  total <- length(y)
  if (!mode %in% c("manual", "auto")) pp_stop("Choose manual or automatic window selection.")
  search <- NULL
  if (mode == "auto") {
    if (total < 2L) pp_stop("Automatic N selection requires at least two observations.")
    if (is.null(search_max) || !length(search_max) || is.na(search_max)) search_max <- floor(total / 2L)
    upper <- pp_integer(search_max, "Search upper limit", 1L, total - 1L)
    evaluation <- seq.int(upper + 1L, total)
    scores <- vapply(seq_len(upper), function(window) {
      pp_metrics(y, pp_ma_fitted(y, window), evaluation)$mse
    }, numeric(1))
    n <- which.min(scores) # Ascending candidates make ties deterministic.
    search <- data.frame(N = seq_len(upper), MSE = scores,
                         Evaluation_start = upper + 1L, Evaluation_end = total)
    accuracy_label <- "N-selection backtest accuracy (not independent test accuracy)"
  } else {
    n <- pp_integer(n, "N", 1L, total)
    evaluation <- if (total > n) seq.int(n + 1L, total) else integer()
    accuracy_label <- "Historical one-step accuracy"
  }
  pp_result("Moving Average", y, pp_ma_fitted(y, n), mean(tail(y, n)),
            list(N = n, Selection = mode), evaluation, accuracy_label,
            details = list(search = search))
}

pp_weighted_average <- function(y, n = 3L, weights = c(0.5, 0.3, 0.2)) {
  y <- pp_series(y)
  n <- pp_integer(n, "N", 1L, length(y))
  if (!is.numeric(weights) || length(weights) != n || any(!is.finite(weights)) ||
      any(weights < 0) || abs(sum(weights) - 1) > 1e-8) {
    pp_stop("Enter exactly N non-negative weights whose sum is 1, most recent period first.")
  }
  fitted <- rep(NA_real_, length(y))
  if (length(y) > n) for (t in seq.int(n + 1L, length(y))) {
    fitted[t] <- sum(weights * rev(y[seq.int(t - n, t - 1L)]))
  }
  pp_result("Weighted Moving Average", y, fitted, sum(weights * rev(tail(y, n))),
            list(N = n, Weights_most_recent_first = paste(weights, collapse = ", ")))
}

pp_phases <- function(n, period, start_phase = 1L) {
  ((seq_len(n) + start_phase - 2L) %% period) + 1L
}

pp_decomposition <- function(y, period, type = "additive", start_phase = 1L) {
  period <- pp_integer(period, "Season length", 2L)
  start_phase <- pp_integer(start_phase, "Starting season", 1L, period)
  if (length(y) < 2L * period) pp_stop(sprintf("Seasonal decomposition requires at least %d observations (two complete cycles).", 2L * period))
  if (!type %in% c("additive", "multiplicative")) pp_stop("Select additive or multiplicative seasonality.")
  if (type == "multiplicative" && any(y <= 0)) pp_stop("Multiplicative seasonality requires strictly positive observations.")
  decomposition <- stats::decompose(stats::ts(y, start = c(1L, start_phase), frequency = period), type = type)
  factors <- as.numeric(decomposition$figure)
  if (any(!is.finite(factors)) || (type == "multiplicative" && any(factors <= 0))) {
    pp_stop("The selected data do not provide usable seasonal components.")
  }
  # decompose() returns its figure in observation order, beginning at the series start.
  component <- rep(factors, length.out = length(y))
  adjusted <- if (type == "additive") y - component else y / component
  list(factors = factors, component = component, adjusted = adjusted,
       trend = as.numeric(decomposition$trend), remainder = as.numeric(decomposition$random),
       period = period, start_phase = start_phase, type = type)
}

pp_regression <- function(values) {
  if (length(values) < 3L) pp_stop("Regression initialization requires at least three initialization observations.")
  time <- seq_along(values)
  model <- stats::lm(values ~ time)
  coefficients <- stats::coef(model)
  if (length(coefficients) != 2L || any(!is.finite(coefficients))) pp_stop("The initialization regression is not identifiable.")
  errors <- as.numeric(stats::residuals(model))
  notes <- "Linear trend is an approximation; inspect the initialization fit and residuals. Normality is not required just to compute an initial point estimate."
  lb <- NA_real_
  if (length(errors) >= 6L && stats::sd(errors) > 1e-10) {
    lag <- min(10L, floor(length(errors) / 3L))
    if (lag > 0L) lb <- stats::Box.test(errors, lag = lag, type = "Ljung-Box")$p.value
    if (is.finite(lb) && lb < 0.05) notes <- paste(notes, "Residual autocorrelation was detected; classical independent-error inference may be unreliable.")
  }
  list(level = unname(coefficients[1] + coefficients[2] * length(values)),
       slope = unname(coefficients[2]), intercept = unname(coefficients[1]),
       fitted = as.numeric(stats::fitted(model)), residuals = errors,
       r_squared = if (sum((values - mean(values))^2) > 1e-20)
         1 - sum(errors^2) / sum((values - mean(values))^2) else NA_real_,
       ljung_box_p = lb, note = notes)
}

pp_initialization <- function(y, method, mode, count, level = NA_real_, slope = NA_real_,
                              period = 1L, type = "additive", start_phase = 1L) {
  seasonal <- method == "winters"
  minimum <- if (seasonal) 2L * period else if (mode == "regression") 3L else 1L
  count <- pp_integer(count, "Initialization observations", minimum, length(y))
  if (!mode %in% c("manual", "regression")) pp_stop("Choose manual or regression initialization.")
  initial <- y[seq_len(count)]
  decomposition <- if (seasonal) pp_decomposition(initial, period, type, start_phase) else NULL
  regression <- NULL
  if (mode == "regression") {
    regression <- pp_regression(if (seasonal) decomposition$adjusted else initial)
    level <- regression$level
    slope <- if (method == "ses") 0 else regression$slope
  } else {
    level <- pp_number(level, "Initial level")
    slope <- if (method == "ses") 0 else pp_number(slope, "Initial slope")
  }
  if (seasonal && type == "multiplicative" && level <= 0) pp_stop("The initial level must be positive for multiplicative Winters.")
  list(count = count, level = level, slope = slope, mode = mode,
       seasonal = if (seasonal) decomposition$factors else numeric(),
       regression = regression,
       initialization_data = if (seasonal) decomposition$adjusted else initial)
}

pp_smoothing_path <- function(y, method, initial, alpha, beta = 0, gamma = 0,
                              period = 1L, type = "additive") {
  n <- length(y)
  k <- initial$count
  level <- initial$level
  slope <- initial$slope
  seasonal <- initial$seasonal
  fitted <- rep(NA_real_, n)
  levels <- slopes <- rep(NA_real_, n)
  levels[k] <- level
  slopes[k] <- slope
  valid <- TRUE
  if (n > k) for (t in seq.int(k + 1L, n)) {
    old_level <- level
    old_slope <- slope
    phase <- ((t - 1L) %% period) + 1L
    if (method == "ses") {
      fitted[t] <- old_level
      level <- alpha * y[t] + (1 - alpha) * old_level
    } else if (method == "holt") {
      fitted[t] <- old_level + old_slope
      level <- alpha * y[t] + (1 - alpha) * fitted[t]
      slope <- beta * (level - old_level) + (1 - beta) * old_slope
    } else {
      old_season <- seasonal[phase]
      base <- old_level + old_slope
      if (type == "multiplicative" && (old_season <= 0 || base <= 0)) { valid <- FALSE; break }
      fitted[t] <- if (type == "additive") base + old_season else base * old_season
      adjusted <- if (type == "additive") y[t] - old_season else y[t] / old_season
      level <- alpha * adjusted + (1 - alpha) * base
      slope <- beta * (level - old_level) + (1 - beta) * old_slope
      if (!is.finite(level) || (type == "multiplicative" && level <= 0)) { valid <- FALSE; break }
      # Use the current-level seasonal update documented by stats::HoltWinters.
      observed_season <- if (type == "additive") y[t] - level else y[t] / level
      seasonal[phase] <- gamma * observed_season + (1 - gamma) * old_season
    }
    levels[t] <- level
    slopes[t] <- slope
    if (!all(is.finite(c(level, slope)))) { valid <- FALSE; break }
  }
  next_phase <- (n %% period) + 1L
  next_forecast <- if (method == "ses") level else if (method == "holt") level + slope else if (type == "additive") {
    level + slope + seasonal[next_phase]
  } else (level + slope) * seasonal[next_phase]
  if (method == "winters" && type == "multiplicative" &&
      (level + slope <= 0 || any(seasonal <= 0))) valid <- FALSE
  list(fitted = fitted, forecast = if (valid) next_forecast else NA_real_,
       level = levels, slope = slopes, seasonal = seasonal, valid = valid)
}

pp_smoothing <- function(y, method = "ses", parameter_mode = "manual",
                         alpha = 0.2, beta = 0.1, gamma = 0.1,
                         init_mode = "regression", init_count = NULL,
                         init_level = NA_real_, init_slope = NA_real_,
                         period = 1L, type = "additive", start_phase = 1L) {
  y <- pp_series(y)
  if (!method %in% c("ses", "holt", "winters")) pp_stop("Unknown smoothing method.")
  if (!parameter_mode %in% c("manual", "auto")) pp_stop("Choose manual or automatic parameter selection.")
  if (method == "winters") {
    period <- pp_integer(period, "Season length", 2L)
    start_phase <- pp_integer(start_phase, "Starting season", 1L, period)
    if (type == "multiplicative" && any(y <= 0)) pp_stop("Multiplicative Winters requires strictly positive observations.")
  } else period <- 1L
  if (is.null(init_count) || !length(init_count) || is.na(init_count)) {
    init_count <- if (method == "winters") 2L * period else if (init_mode == "manual") 1L else 3L
  }
  initial <- pp_initialization(y, method, init_mode, init_count, init_level, init_slope,
                               period, type, start_phase)
  number <- switch(method, ses = 1L, holt = 2L, winters = 3L)
  parameters <- c(alpha = alpha, beta = beta, gamma = gamma)[seq_len(number)]
  optimization <- NULL
  if (parameter_mode == "auto") {
    if (length(y) <= initial$count) pp_stop("Automatic parameter selection needs observations after initialization.")
    objective <- function(p) {
      values <- c(p, rep(0, 3L - length(p)))
      path <- pp_smoothing_path(y, method, initial, values[1], values[2], values[3], period, type)
      if (!path$valid || !is.finite(path$forecast)) return(1e100)
      errors <- y - path$fitted
      value <- sum(errors[is.finite(errors)]^2)
      if (is.finite(value)) value else 1e100
    }
    if (number == 1L) {
      candidates <- c(0, stats::optimize(objective, c(0, 1), tol = 1e-8)$minimum, 1)
      scores <- vapply(candidates, objective, numeric(1))
      parameters <- c(alpha = candidates[which.min(scores)])
      optimization <- min(scores)
    } else {
      fits <- lapply(c(0.1, 0.5, 0.9), function(start) {
        tryCatch(stats::optim(rep(start, number), objective, method = "L-BFGS-B",
                              lower = rep(0, number), upper = rep(1, number)), error = function(e) NULL)
      })
      fits <- Filter(function(fit) !is.null(fit) && fit$convergence == 0 &&
                       is.finite(fit$value) && fit$value < 1e99, fits)
      if (!length(fits)) pp_stop("Parameter optimization did not converge. Try manual parameters or different initialization values.")
      best <- fits[[which.min(vapply(fits, function(fit) fit$value, numeric(1)))]]
      parameters <- stats::setNames(best$par, c("alpha", "beta", "gamma")[seq_len(number)])
      optimization <- best$value
    }
  } else for (name in names(parameters)) parameters[[name]] <- pp_number(parameters[[name]], name, 0, 1)
  values <- c(unname(parameters), rep(0, 3L - number))
  path <- pp_smoothing_path(y, method, initial, values[1], values[2], values[3], period, type)
  if (!path$valid) pp_stop("These parameters lead to an invalid smoothing state. Try an additive model or different parameters and initialization.")
  label <- switch(method, ses = "Simple Exponential Smoothing", holt = "Holt's Method", winters = "Winters' Method")
  params <- c(as.list(parameters), list(Parameter_selection = parameter_mode,
              Initialization = init_mode, Initialization_observations = initial$count,
              Initial_level = initial$level))
  if (method != "ses") params$Initial_slope <- initial$slope
  if (method == "winters") params <- c(params, list(Season_length = period, Seasonality = type))
  notes <- if (!is.null(initial$regression)) initial$regression$note else character()
  if (parameter_mode == "auto") notes <- c(notes, "Optimization minimizes historical squared errors with fixed initialization; it is not an independent forecast test or a guarantee of a global optimum.")
  pp_result(label, y, path$fitted, path$forecast, params,
            accuracy_label = if (parameter_mode == "auto") "Parameter-fitting accuracy (not independent test accuracy)" else "Historical one-step accuracy after initialization",
            details = list(initialization = initial, states = path, optimization_sse = optimization), warnings = notes)
}

pp_seasonal_factors <- function(y, period = 4L, start_phase = 1L) {
  y <- pp_series(y)
  period <- pp_integer(period, "Season length", 2L)
  start_phase <- pp_integer(start_phase, "Starting season", 1L, period)
  if (length(y) < 2L * period) pp_stop(sprintf("Quick and Dirty seasonal factors require at least %d observations (two cycles).", 2L * period))
  if (any(y <= 0)) pp_stop("Ratio seasonal factors require strictly positive observations. Use additive seasonal decomposition for data containing zeros or negative values.")
  phase <- pp_phases(length(y), period, start_phase)
  raw <- as.numeric(tapply(y / mean(y), factor(phase, levels = seq_len(period)), mean))
  factors <- raw / mean(raw)
  adjusted <- y / factors[phase]
  baseline <- mean(adjusted)
  fitted <- baseline * factors[phase]
  next_phase <- ((length(y) + start_phase - 1L) %% period) + 1L
  pp_result("Seasonal Factors - Quick and Dirty", y, fitted, baseline * factors[next_phase],
            list(Season_length = period, Starting_season = start_phase, Deseasonalized_level = baseline),
            accuracy_label = "In-sample seasonal fit (not independent test accuracy)",
            details = list(seasonal = data.frame(Season = seq_len(period), Factor = factors),
                           decomposition = data.frame(Position = seq_along(y), Actual = y,
                                                       Adjusted = adjusted, Seasonal = factors[phase])),
            warnings = "Quick and Dirty estimates a repeating seasonal pattern around a level; it does not model a trend. Factors are normalized to have mean 1.")
}

pp_seasonal_decomposition <- function(y, period = 4L, type = "additive", start_phase = 1L) {
  y <- pp_series(y)
  decomposition <- pp_decomposition(y, period, type, start_phase)
  regression <- pp_regression(decomposition$adjusted)
  next_base <- regression$level + regression$slope
  next_factor <- decomposition$factors[(length(y) %% period) + 1L]
  fitted <- if (type == "additive") regression$fitted + decomposition$component else regression$fitted * decomposition$component
  if (type == "multiplicative" && (any(regression$fitted <= 0) || next_base <= 0)) {
    pp_stop("The fitted trend becomes non-positive. Use additive decomposition or a different method.")
  }
  next_forecast <- if (type == "additive") next_base + next_factor else next_base * next_factor
  pp_result("Seasonal Decomposition with Moving Averages", y, fitted, next_forecast,
            list(Season_length = period, Seasonality = type,
                 Trend_intercept = regression$intercept, Trend_slope = regression$slope),
            accuracy_label = "In-sample decomposition/trend fit (not independent test accuracy)",
            details = list(seasonal = data.frame(Season = pp_phases(period, period, start_phase),
                                                Factor = decomposition$factors),
                           regression = regression,
                           decomposition = data.frame(Position = seq_along(y), Actual = y,
                             Trend = decomposition$trend, Seasonal = decomposition$component,
                             Adjusted = decomposition$adjusted, Remainder = decomposition$remainder)),
            warnings = c(regression$note, "Centered moving averages have unavailable trend values at the ends. Seasonal factors use the fitted sample; the displayed historical fit is not a real-time forecast test."))
}

pp_box_jenkins <- function(y, family = "arima", p = 1L, d = 1L, q = 1L,
                           include_constant = TRUE, period = 1L, start_phase = 1L,
                           seasonal_search = FALSE, max_p = 5L, max_q = 5L,
                           search_mode = "thorough") {
  y <- pp_series(y)
  if (!requireNamespace("forecast", quietly = TRUE)) pp_stop("The forecast package is required for Box-Jenkins models.")
  if (length(y) < 4L) pp_stop("Box-Jenkins estimation requires at least four observations, and more for higher-order models.")
  if (stats::sd(y) < 1e-12) pp_stop("The series is constant. ACF/PACF and Box-Jenkins estimation are not informative; use a level forecasting method.")
  period <- pp_integer(period, "Season length", 1L)
  if (period > 1L) start_phase <- pp_integer(start_phase, "Starting season", 1L, period) else start_phase <- 1L
  x <- stats::ts(y, start = c(1L, start_phase), frequency = period)
  captured <- character()
  model <- withCallingHandlers({
    if (family == "auto_arima") {
      max_p <- pp_integer(max_p, "Maximum p", 0L, 10L)
      max_q <- pp_integer(max_q, "Maximum q", 0L, 10L)
      if (!search_mode %in% c("thorough", "fast")) pp_stop("Choose thorough or fast ARIMA search.")
      seasonal_search <- isTRUE(seasonal_search) && period > 1L
      if (seasonal_search && length(y) < 2L * period + 1L) pp_stop("Seasonal ARIMA search needs more than two complete cycles.")
      forecast::auto.arima(x, max.p = max_p, max.q = max_q,
                           start.p = min(2L, max_p), start.q = min(2L, max_q),
                           max.P = if (seasonal_search) 2L else 0L,
                           max.Q = if (seasonal_search) 2L else 0L,
                           max.order = 5L, max.d = 2L, max.D = if (seasonal_search) 1L else 0L,
                           seasonal = seasonal_search, ic = "aicc",
                           allowmean = isTRUE(include_constant), allowdrift = isTRUE(include_constant),
                           stepwise = search_mode == "fast", approximation = FALSE)
    } else {
      if (!family %in% c("ar", "ma", "arma", "arima")) pp_stop("Unknown Box-Jenkins family.")
      p <- if (family == "ma") 0L else pp_integer(p, "p", if (family %in% c("ar", "arma")) 1L else 0L, 20L)
      q <- if (family == "ar") 0L else pp_integer(q, "q", if (family %in% c("ma", "arma")) 1L else 0L, 20L)
      d <- if (family == "arima") pp_integer(d, "d", 0L, 2L) else 0L
      estimated <- p + q + as.integer(isTRUE(include_constant) && d <= 1L) + 1L
      if (length(y) - d <= estimated + 1L) pp_stop("There are too few observations for these orders. Reduce p/q/d or provide more data.")
      forecast::Arima(x, order = c(p, d, q), seasonal = c(0L, 0L, 0L),
                      include.mean = isTRUE(include_constant) && d == 0L,
                      include.drift = isTRUE(include_constant) && d == 1L, method = "ML")
    }
  }, warning = function(w) { captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (!is.null(model$code) && model$code != 0L) pp_stop("ARIMA estimation did not converge. Reduce the model orders or try a different family.")
  if (any(!is.finite(stats::coef(model))) || !is.finite(model$sigma2)) pp_stop("ARIMA estimation did not return finite parameters.")
  predictions <- forecast::forecast(model, h = 1L, level = c(80, 95))
  fitted <- as.numeric(stats::fitted(model))
  order <- forecast::arimaorder(model)
  differences <- unname(order["d"])
  seasonal_d <- if ("D" %in% names(order)) unname(order["D"]) else 0L
  transformed <- y
  if (seasonal_d > 0L) transformed <- diff(transformed, lag = period, differences = seasonal_d)
  if (differences > 0L) transformed <- diff(transformed, differences = differences)
  exclude <- differences + seasonal_d * period
  if (exclude > 0L) fitted[seq_len(min(exclude, length(y)))] <- NA_real_
  coefficients <- stats::coef(model)
  standard_errors <- if (length(coefficients)) {
    variance <- diag(model$var.coef)
    ifelse(is.finite(variance) & variance >= 0, sqrt(pmax(0, variance)), NA_real_)
  } else numeric()
  residuals <- as.numeric(stats::residuals(model))
  residuals <- residuals[is.finite(residuals)]
  fit_df <- sum(order[intersect(c("p", "q", "P", "Q"), names(order))])
  lag <- min(if (period > 1L) 2L * period else 10L, floor(length(residuals) / 5L))
  ljung <- if (lag > fit_df && stats::sd(residuals) > 1e-12) {
    stats::Box.test(residuals, lag = lag, fitdf = fit_df, type = "Ljung-Box")$p.value
  } else NA_real_
  parameters <- c(as.list(order), list(Constant_allowed = isTRUE(include_constant),
                                      AIC = model$aic, AICc = model$aicc, BIC = model$bic,
                                      Residual_variance = model$sigma2))
  if (family == "auto_arima") parameters <- c(parameters, list(Selection = "AICc",
     Search = search_mode, Maximum_p = max_p, Maximum_q = max_q, Maximum_total_order = 5L,
     Seasonal_search = seasonal_search))
  label <- switch(family, ar = "AR", ma = "MA (Box-Jenkins)", arma = "ARMA",
                  arima = "ARIMA", auto_arima = "Automatic ARIMA Selection")
  notes <- c(captured, "Historical fitted accuracy uses parameters estimated on the sample. Prediction intervals are model-based and do not guarantee coverage.")
  if (family == "auto_arima") notes <- c(notes, "Automatic selection minimizes AICc within the displayed search limits; it does not guarantee the lowest future forecast error.")
  if (is.finite(ljung) && ljung < 0.05) notes <- c(notes, "Residual autocorrelation remains significant in the Ljung-Box check; review the model specification.")
  pp_result(label, y, fitted, as.numeric(predictions$mean[1]), parameters,
            accuracy_label = "In-sample model fit (not independent test accuracy)",
            details = list(model = model, transformed = transformed,
                estimates = data.frame(Parameter = names(coefficients), Estimate = unname(coefficients),
                                        Standard_error = unname(standard_errors)),
                intervals = data.frame(Level = c("80%", "95%"),
                  Lower = as.numeric(predictions$lower[1, ]), Upper = as.numeric(predictions$upper[1, ])),
                residuals = residuals, ljung_box_p = ljung, ljung_box_lag = lag), warnings = notes)
}

pp_catalog <- function() {
  data.frame(id = c("moving_average", "weighted_average", "ses", "seasonal_factors", "holt",
                    "decomposition", "winters", "ar", "ma", "arma", "arima", "auto_arima"),
             label = c("Moving Average", "Weighted Moving Average", "Simple Exponential Smoothing",
                        "Seasonal Factors - Quick and Dirty", "Holt's Method",
                        "Seasonal Decomposition with Moving Averages", "Winters' Method", "AR",
                        "MA (Box-Jenkins)", "ARMA", "ARIMA", "Automatic ARIMA Selection"),
             group = c(rep("Stationary Series", 4), "Trend Series", rep("Seasonal Series", 2),
                       rep("Box-Jenkins", 5)), stringsAsFactors = FALSE)
}

pp_fit <- function(method, y, params = list()) {
  value <- function(name, default = NULL) if (is.null(params[[name]])) default else params[[name]]
  switch(method,
    moving_average = pp_moving_average(y, value("n", 3L), value("parameter_mode", "manual"), value("search_max")),
    weighted_average = pp_weighted_average(y, value("n", 3L), value("weights", c(0.5, 0.3, 0.2))),
    ses = , holt = , winters = pp_smoothing(y, method, value("parameter_mode", "manual"),
      value("alpha", 0.2), value("beta", 0.1), value("gamma", 0.1), value("init_mode", "regression"),
      value("init_count"), value("init_level", NA_real_), value("init_slope", NA_real_),
      value("period", 1L), value("type", "additive"), value("start_phase", 1L)),
    seasonal_factors = pp_seasonal_factors(y, value("period", 4L), value("start_phase", 1L)),
    decomposition = pp_seasonal_decomposition(y, value("period", 4L), value("type", "additive"), value("start_phase", 1L)),
    ar = , ma = , arma = , arima = , auto_arima = pp_box_jenkins(y, method,
      value("p", 1L), value("d", 1L), value("q", 1L), value("include_constant", TRUE),
      value("period", 1L), value("start_phase", 1L), value("seasonal_search", FALSE),
      value("max_p", 5L), value("max_q", 5L), value("search_mode", "thorough")),
    pp_stop("Unknown forecasting method."))
}
