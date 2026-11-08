ap_unit_suffix_pattern <- "\\s*(?:\\(([^()]*)\\)|\\[([^\\[\\]]*)\\])\\s*$"

ap_header_key <- function(header) {
  gsub("[^a-z0-9]", "", tolower(header))
}

ap_days_headers <- c("daycount", "days", "workingday", "workingdays", "workday", "workdays",
                     "businessdays", "numberofworkingdays", "numberofworkdays")
ap_hours_headers <- c("workinghour", "workinghours", "workhour", "workhours", "numberofworkinghours")

ap_guess_columns <- function(data) {
  columns <- names(data)
  keys <- ap_header_key(sub(ap_unit_suffix_pattern, "", columns, perl = TRUE))
  roles <- list(
    period = c("period", "periodname", "periodnames", "periodlabel", "month", "monthname",
               "quarter", "week", "year", "day", "date", "time"),
    demand = c("demand", "demandforecast", "demandforecasts", "forecastdemand", "forecasteddemand",
               "forecast", "forecasts"),
    days = c(ap_days_headers, ap_hours_headers, "workingtime", "worktime")
  )
  vapply(roles, function(aliases) {
    candidates <- columns[keys %in% aliases]
    if (length(candidates) == 1L) candidates else ""
  }, character(1))
}

ap_normalize_unit <- function(value, explicit = FALSE) {
  value <- gsub("[[:space:]]+", " ", trimws(value))
  key <- tolower(chartr("\u0130\u0131\u00dc", "ii\u00fc", value))
  if (is.na(key) || key %in% c("", "na", "n/a", "unknown", "-")) return("")
  aliases <- list(
    units = c("unit", "units", "ea", "each", "adet"),
    `aggregate units` = c("aggregate unit", "aggregate units"),
    pieces = c("piece", "pieces", "pc", "pcs"),
    cases = c("case", "cases", "koli"),
    boxes = c("box", "boxes", "kutu"),
    days = c("d", "day", "days", "working day", "working days", "workday", "workdays", "business day",
             "business days", "g\u00fcn", "gun"),
    hours = c("h", "hr", "hrs", "hour", "hours", "working hour", "working hours", "work hours", "saat"),
    minutes = c("min", "mins", "minute", "minutes"),
    seconds = c("sec", "secs", "second", "seconds"),
    weeks = c("wk", "wks", "week", "weeks"),
    months = c("month", "months"),
    shifts = c("shift", "shifts"),
    kg = c("kg", "kilogram", "kilograms"),
    lb = c("lb", "lbs", "pound", "pounds"),
    tonnes = c("tonne", "tonnes", "metric ton", "metric tons")
  )
  for (unit in names(aliases)) {
    if (key %in% aliases[[unit]]) return(unit)
  }
  if (grepl(paste0("^(?:(?:thousands?|millions?|hundreds?) (?:of )?|[0-9]+ ?x? )?",
                                "(?:units?|pieces?|cases?|boxes?|pallets?|packs?|pairs?|bottles?|",
                                "tons?|grams?|kilograms?|kg|pounds?|liters?|litres?|gallons?|hours?|minutes?|",
                                "calendar days?|worker[- ]days?|worker[- ]hours?)$|^(?:1000s|000s)$"), key, perl = TRUE)) return(key)
  if (explicit) return(value)
  ""
}

ap_detect_units <- function(data, demand_column, days_column) {
  columns <- names(data)
  keys <- ap_header_key(columns)
  detect <- function(column, metadata_keys, infer_time_unit = FALSE) {
    if (length(column) != 1L || is.na(column) || !column %in% columns) return("")
    match <- regmatches(column, regexec(ap_unit_suffix_pattern, column, perl = TRUE))[[1L]]
    unit_hint <- if (length(match)) trimws(paste0(match[2L], match[3L])) else ""
    suffix <- ap_normalize_unit(unit_hint)
    metadata <- unlist(lapply(data[keys %in% metadata_keys], as.character), use.names = FALSE)
    declared <- c(suffix, vapply(metadata, ap_normalize_unit, character(1), explicit = TRUE, USE.NAMES = FALSE))
    declared <- declared[nzchar(declared) & !duplicated(tolower(declared))]
    if (length(declared) == 1L) return(declared)
    if (!length(declared) && infer_time_unit && !nzchar(unit_hint)) {
      if (ap_header_key(column) %in% ap_days_headers) return("days")
      if (ap_header_key(column) %in% ap_hours_headers) return("hours")
    }
    ""
  }
  c(demand = detect(demand_column, c("demandunit", "demandunits", "demandforecastunit",
                                    "demandforecastunits", "forecastunit", "forecastunits")),
    days = detect(days_column, c("workingdayunit", "workingdaysunit", "workingdaysunits",
                                "workdaysunit", "workdaysunits", "businessdaysunit", "businessdaysunits",
                                "daysunit", "daysunits", "workingtimeunit", "workingtimeunits",
                                "worktimeunit", "worktimeunits", "workinghoursunit", "workinghoursunits",
                                "workhoursunit", "workhoursunits"), infer_time_unit = TRUE))
}

ap_prepare_periods <- function(data, period_column, demand_column, days_column, decimal = ".", days_factor = 1) {
  if (!is.data.frame(data) || !nrow(data)) pp_stop("Upload a planning dataset with at least one period.")
  selected <- list(period_column, demand_column, days_column)
  valid_column <- function(column) {
    is.character(column) && length(column) == 1L && !is.na(column) && nzchar(column) &&
      sum(names(data) == column, na.rm = TRUE) == 1L
  }
  if (!all(vapply(selected, valid_column, logical(1))) || anyDuplicated(unlist(selected))) {
    pp_stop("Select three different columns for period, demand and working time.")
  }
  if (length(decimal) != 1L || !decimal %in% c(".", ",")) pp_stop("Select a valid decimal mark.")
  if (!is.numeric(days_factor) || length(days_factor) != 1L || !is.finite(days_factor) || days_factor <= 0) {
    pp_stop("The working time to days multiplier must be a finite positive number.")
  }
  rows <- function(indices) paste(head(indices, 12L), collapse = ", ")
  parse_amount <- function(values, label) {
    if (is.factor(values)) values <- as.character(values)
    if (is.character(values)) {
      values <- trimws(values)
      mark <- if (decimal == ".") "\\." else ","
      pattern <- paste0("^[+-]?(?:[0-9]+(?:", mark, "[0-9]*)?|", mark, "[0-9]+)(?:[eE][+-]?[0-9]+)?$")
      values[!grepl(pattern, values, perl = TRUE)] <- NA_character_
      if (decimal == ",") values <- sub(",", ".", values, fixed = TRUE)
      values <- as.numeric(values)
    }
    if (!is.numeric(values) || !is.null(dim(values)) || inherits(values, c("Date", "POSIXt", "difftime"))) {
      pp_stop(sprintf("%s must be numeric.", label))
    }
    bad <- which(!is.finite(values) | values < 0)
    if (length(bad)) pp_stop(sprintf("%s must be finite and non-negative at rows: %s.", label, rows(bad)))
    as.numeric(values)
  }
  period <- trimws(as.character(data[[period_column]]))
  blank <- which(is.na(period) | !nzchar(period))
  if (length(blank)) pp_stop(sprintf("Period names are missing at rows: %s.", rows(blank)))
  duplicates <- which(duplicated(tolower(period)))
  if (length(duplicates)) pp_stop(sprintf("Period names must be unique. Repeated names at rows: %s.", rows(duplicates)))
  demand <- parse_amount(data[[demand_column]], "Demand")
  days <- parse_amount(data[[days_column]], "Working time") * days_factor
  bad_days <- which(!is.finite(days) | abs(days - round(days)) > 1e-8)
  if (length(bad_days)) pp_stop(sprintf("Working days must be whole numbers after conversion at rows: %s.", rows(bad_days)))
  data.frame(period = period, demand = demand, days = round(days), stringsAsFactors = FALSE)
}

ap_productivity <- function(units, workers, days) {
  if (!length(units) || length(units) != length(workers) || length(units) != length(days)) {
    pp_stop("Enter equally sized, non-empty histories for units produced, workers and working days.")
  }
  valid <- function(x) is.numeric(x) && is.null(dim(x)) && all(is.finite(x))
  if (!valid(units) || any(units < 0)) pp_stop("Historical units produced must be finite and non-negative.")
  if (!valid(workers) || !valid(days) || any(workers <= 0) || any(days <= 0)) {
    pp_stop("Historical workers and working days must be finite and positive for each row.")
  }
  productivity <- sum(units) / sum(as.numeric(workers) * as.numeric(days))
  if (!is.finite(productivity) || productivity <= 0) pp_stop("Historical data must produce a finite positive productivity.")
  productivity
}
