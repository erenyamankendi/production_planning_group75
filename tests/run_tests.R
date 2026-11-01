# Method-level numerical and Shiny-module checks. No publishing or commits.
local({
  args <- grep("^--file=", commandArgs(), value = TRUE)
  source_files <- vapply(sys.frames(), function(frame) if (is.character(frame$ofile)) frame$ofile else "", character(1))
  source_files <- source_files[nzchar(source_files)]
  script <- if (length(source_files)) normalizePath(tail(source_files,1L)) else if (length(args)) normalizePath(sub("^--file=", "", args[1])) else normalizePath("tests/run_tests.R")
  app_dir <- if (basename(script) == "run_tests.R") dirname(dirname(script)) else dirname(script)
  workspace <- dirname(app_dir)
  version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
  .libPaths(c(file.path(workspace, ".R-library"),
    file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", version),
    file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", version), .libPaths()))
  cat("Test runtime:", workspace, "|", paste(.libPaths(), collapse = " | "), "\n")
  for (file in list.files(file.path(app_dir, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(file, envir = environment())
  passed <- 0L
  failures <- character()
  check <- function(name, code) {
    result <- tryCatch({ force(code); NULL }, error = identity)
    if (is.null(result)) { passed <<- passed + 1L; cat("PASS", name, "\n") }
    else { failures <<- c(failures, paste(name, conditionMessage(result), sep = ": ")); cat("FAIL", tail(failures, 1L), "\n") }
  }
  close_to <- function(actual, expected, tolerance = 1e-8) {
    if (is.numeric(actual) && is.numeric(expected)) { actual <- as.numeric(actual); expected <- as.numeric(expected) }
    if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
      stop(paste("Expected", paste(expected, collapse = ","), "but got", paste(actual, collapse = ",")))
    }
  }
  fails <- function(code, pattern = NULL) {
    error <- tryCatch({ force(code); NULL }, error = identity)
    if (!inherits(error, "error")) stop("Expected a validation error.")
    if (!is.null(pattern) && !grepl(pattern, conditionMessage(error), fixed = TRUE)) stop(conditionMessage(error))
  }
  y <- c(10, 20, 30, 20, 40, 30)
  check("MA manual reference and untouched warm-up periods", {
    result <- pp_moving_average(y, n = 3)
    close_to(result$fitted, c(NA, NA, NA, 20, 70/3, 30))
    close_to(result$forecast, 30)
    close_to(c(result$metrics$mad, result$metrics$mse, result$metrics$mape), c(50/9, 2500/27, 125/9))
    stopifnot(identical(result$metrics$periods, 4:6))
  })
  check("MA automatic search uses the same periods for every candidate", {
    result <- pp_moving_average(y, mode = "auto", search_max = 3)
    close_to(result$details$search$MSE, c(200, 250/3, 2500/27))
    close_to(result$parameters$N, 2)
    close_to(result$forecast, 35)
    stopifnot(all(result$details$search$Evaluation_start == 4))
    close_to(pp_moving_average(rep(7, 8), mode = "auto")$parameters$N, 1)
  })
  check("MA boundary windows and explicit invalid-input handling", {
    result <- pp_moving_average(y, n = length(y))
    close_to(result$forecast, 25)
    stopifnot(result$metrics$count == 0L, is.na(result$metrics$mse))
    close_to(pp_moving_average(5, n = 1)$forecast, 5)
    fails(pp_moving_average(y, 2.5), "integer")
    fails(pp_moving_average(y, 7), "integer")
    fails(pp_moving_average(5, mode = "auto"), "two")
    fails(pp_moving_average(c(1, NA, 2), 1), "Missing")
    altered <- y; altered[4] <- 999
    close_to(pp_moving_average(altered, 3)$fitted[4], 20)
  })
  check("Zero actual values keep MAD/MSE but make MAPE unavailable", {
    result <- pp_moving_average(c(1, 0, 2), 1)
    stopifnot(is.na(result$metrics$mape), result$metrics$count == 2L)
    close_to(result$metrics$mad, 1.5)
    close_to(result$metrics$mse, 2.5)
    result <- pp_moving_average(rep(0, 8), mode = "auto")
    close_to(result$forecast, 0)
    close_to(result$metrics$mse, 0)
    stopifnot(is.na(result$metrics$mape))
  })
  check("Weighted MA orientation and weight validation", {
    result <- pp_weighted_average(y, 3, c(0.5, 0.3, 0.2))
    close_to(result$forecast, 31)
    close_to(result$fitted[4], 23)
    fails(pp_weighted_average(y, 3, c(0.5, 0.3, 0.3)), "sum")
    fails(pp_weighted_average(y, 3, c(1.1, -0.1, 0)), "non-negative")
  })
  check("SES agrees with the R reference under matching manual initialization", {
    result <- pp_smoothing(y, "ses", alpha = 0.5, init_mode = "manual", init_count = 1, init_level = 10)
    reference <- stats::HoltWinters(y, alpha = 0.5, beta = FALSE, gamma = FALSE, l.start = 10)
    close_to(result$fitted[-1], reference$fitted[, "xhat"])
    close_to(result$forecast, as.numeric(predict(reference, n.ahead = 1)))
    close_to(result$forecast, 30.3125)
    close_to(pp_smoothing(y, "ses", alpha = 0, init_mode = "manual", init_level = 10)$forecast, 10)
    close_to(pp_smoothing(y, "ses", alpha = 1, init_mode = "manual", init_level = 10)$forecast, tail(y, 1))
  })
  trend <- 10 + 2 * (1:24)
  check("Manual and automatic Holt retain the trend component", {
    for (mode in c("manual", "auto")) {
      result <- pp_smoothing(trend, "holt", parameter_mode = mode, alpha = 0.4, beta = 0.2,
                             init_mode = "manual", init_count = 2, init_level = 14, init_slope = 2)
      close_to(result$forecast, 60)
      close_to(result$metrics$mse, 0, 1e-7)
      stopifnot("beta" %in% names(result$parameters))
    }
    result <- pp_smoothing(trend, "holt", alpha = 0.4, beta = 0.2,
                           init_mode = "manual", init_count = 2, init_level = 14, init_slope = 2)
    reference <- stats::HoltWinters(trend, alpha = 0.4, beta = 0.2, gamma = FALSE, l.start = 14, b.start = 2)
    close_to(result$fitted[-c(1,2)], reference$fitted[, "xhat"])
  })
  check("Regression initialization uses only the declared prefix", {
    result <- pp_smoothing(trend, "holt", init_count = 3)
    close_to(result$details$initialization$level, 16)
    close_to(result$details$initialization$slope, 2)
    altered <- trend; altered[20] <- 1000
    other <- pp_smoothing(altered, "holt", init_count = 3)
    close_to(result$details$initialization$level, other$details$initialization$level)
    close_to(result$details$initialization$slope, other$details$initialization$slope)
    stopifnot(all(is.na(result$fitted[1:3])))
    fails(pp_smoothing(c(1,2), "ses", init_mode = "regression"), "Initialization")
  })
  seasonal <- 100 + 2 * (1:24) + rep(c(-10,0,10,0), 6)
  check("Quick and Dirty normalizes factors and restores the proper season", {
    data <- rep(c(80,100,120,100), 3)
    result <- pp_seasonal_factors(data, 4)
    close_to(result$details$seasonal$Factor, c(0.8,1,1.2,1))
    close_to(sum(result$details$seasonal$Factor), 4)
    close_to(result$forecast, 80)
    rotated <- pp_seasonal_factors(rep(c(100,120,100,80), 3), 4, start_phase = 2)
    close_to(rotated$details$seasonal$Factor, c(0.8,1,1.2,1))
    close_to(rotated$forecast, 100)
    fails(pp_seasonal_factors(c(rep(1, 7),0), 4), "positive")
  })
  check("Classical decomposition handles even/odd cycles without fabricated endpoints", {
    result <- pp_seasonal_decomposition(seasonal, 4, "additive")
    close_to(sum(result$details$seasonal$Factor), 0)
    close_to(result$forecast, 140)
    stopifnot(is.na(result$details$decomposition$Trend[1]), is.na(tail(result$details$decomposition$Trend,1)))
    odd <- 50 + (1:18) + rep(c(-3,0,3),6)
    close_to(pp_seasonal_decomposition(odd, 3)$forecast, 66)
    positive <- rep(c(80,100,120,100), 4)
    result <- pp_seasonal_decomposition(positive, 4, "multiplicative")
    close_to(mean(result$details$seasonal$Factor), 1)
    close_to(result$forecast, 80)
  })
  check("Winters manual/regression initialization and seasonal alignment", {
    for (mode in c("manual", "regression")) {
      result <- pp_smoothing(seasonal, "winters", alpha = 0.3, beta = 0.2, gamma = 0.1,
        init_mode = mode, init_count = 8, init_level = 116, init_slope = 2, period = 4)
      close_to(result$forecast, 140)
      close_to(result$metrics$mse, 0, 1e-7)
      close_to(sum(result$details$initialization$seasonal), 0)
    }
    positive <- (100 + 2*(1:24))*rep(c(0.8,1,1.2,1),6)
    result <- pp_smoothing(positive, "winters", alpha = 0.3, beta = 0.2, gamma = 0.1,
      init_mode = "manual", init_count = 8, init_level = 116, init_slope = 2, period = 4, type = "multiplicative")
    initial <- result$details$initialization
    reference <- stats::HoltWinters(ts(positive[5:24],frequency=4), alpha=0.3,beta=0.2,gamma=0.1,
      seasonal="multiplicative",l.start=initial$level,b.start=initial$slope,s.start=initial$seasonal)
    close_to(result$fitted[9:24], reference$fitted[,"xhat"])
    close_to(result$forecast,as.numeric(predict(reference,n.ahead=1)))
    fails(pp_smoothing(seasonal - 200,"winters",period=4,type="multiplicative"), "positive")
  })
  check("Automatic smoothing parameters are finite and bounded", {
    data <- seasonal + rep(c(0,2,-1,1,0,-2),4)
    for (method in c("ses","holt","winters")) {
      result <- pp_smoothing(data, method, parameter_mode="auto", period=4)
      stopifnot(is.finite(result$forecast))
      parameters <- unlist(result$parameters[intersect(c("alpha","beta","gamma"),names(result$parameters))])
      stopifnot(all(parameters >= 0 & parameters <= 1))
    }
  })
  set.seed(341)
  stationary <- as.numeric(stats::arima.sim(list(ar=0.55,ma=0.2),n=100)) + 30
  check("Box-Jenkins families enforce their advertised orders", {
    for (family in c("ar","ma","arma","arima")) {
      result <- pp_box_jenkins(stationary, family, p=1,d=1,q=1)
      orders <- forecast::arimaorder(result$details$model)
      stopifnot(is.finite(result$forecast), length(result$fitted)==length(stationary),nrow(result$details$estimates)>0)
      if (family=="ar") stopifnot(orders['p']==1,orders['q']==0,orders['d']==0)
      if (family=="ma") stopifnot(orders['p']==0,orders['q']==1,orders['d']==0)
      if (family=="arma") stopifnot(orders['p']==1,orders['q']==1,orders['d']==0)
      if (family=="arima") stopifnot(orders['d']==1,length(result$details$transformed)==99)
      stopifnot(all(result$details$intervals$Lower <= result$forecast),all(result$details$intervals$Upper >= result$forecast))
    }
    random_walk <- pp_box_jenkins(y,"arima",p=0,d=1,q=0,include_constant=FALSE)
    close_to(random_walk$forecast, tail(y,1))
    fails(pp_box_jenkins(rep(5,20),"ar"),"constant")
  })
  check("Automatic ARIMA reports its bounded AICc search and usable diagnostics", {
    result <- pp_box_jenkins(stationary,"auto_arima",max_p=2,max_q=2,search_mode="thorough")
    orders <- forecast::arimaorder(result$details$model)
    stopifnot(orders['p']<=2,orders['q']<=2,result$parameters$Selection=="AICc",is.finite(result$forecast),
              is.finite(result$parameters$AICc),length(result$details$residuals)>0)
  })
  check("Time validation distinguishes calendar months, duplicates and missing periods", {
    dates <- as.Date(c("2026-01-31","2026-02-28","2026-03-31"))
    stopifnot(tail(pp_index(dates,3,"month")$labels,1)=="2026-04")
    fails(pp_index(c(1,1,2),3),"Duplicate")
    fails(pp_index(c(1,3,4),3),"irregular")
    fails(pp_index(c(3,2,1),3),"ascending")
    stopifnot(tail(pp_index(NULL,3)$labels,1)=="4")
  })
  check("CSV import preserves numeric values, header names and explicit decimal conventions", {
    path <- tempfile(fileext=".csv"); on.exit(unlink(path),add=TRUE)
    writeLines(c('Period;Demand','1;10,5','2;12,5'),path)
    data <- pp_read_dataset(path,"example.csv",separator="auto",decimal=",")
    close_to(data$Demand,c(10.5,12.5))
    writeLines(c('Period,Demand','1,10','2,20'),path)
    close_to(pp_read_dataset(path,"example.csv")$Demand,c(10,20))
    writeLines(c('Period,Period','1,10'),path)
    fails(pp_read_dataset(path,"example.csv"),"unique")
  })
  check("Every method UI can be constructed with namespaced controls", {
    for (method in pp_catalog()$id) {
      html <- htmltools::renderTags(forecasting_method_ui(paste0("check_",method),method))$html
      stopifnot(grepl(paste0("check_",method,"-run"),html,fixed=TRUE))
    }
    htmltools::renderTags(forecasting_ui("forecasting"))
  })
  check("Excel import reads the selected worksheet and preserves its numeric values", {
    path <- readxl::readxl_example("datasets.xlsx")
    data <- pp_read_dataset(path,"datasets.xlsx",sheet="mtcars")
    stopifnot(nrow(data)==32L)
    close_to(data$mpg[1],21)
  })
  check("Data editing maps DT columns correctly and deletion uses the confirmed selection", {
    shiny::testServer(data_management_server,{
      session$setInputs(period_column="Period",target="Demand",time_unit="period")
      session$setInputs(add_row=1)
      stopifnot(nrow(rv$data)==1L,is.na(rv$data$Demand[1]),rv$data$Period[1]==1)
      session$setInputs(table_cell_edit=list(row=1L,col=1L,value="10"))
      close_to(rv$data$Demand[1],10)
      close_to(bundle()$y,10)
      session$setInputs(add_row=2)
      session$setInputs(table_cell_edit=list(row=2L,col=1L,value="20"))
      close_to(bundle()$y,c(10,20))
      session$setInputs(table_rows_selected=1L,delete_rows=1)
      session$setInputs(table_rows_selected=2L,confirm_delete=1)
      close_to(rv$data$Demand,20)
      session$setInputs(table_rows_selected=1L,delete_rows=2)
      session$setInputs(add_row=3)
      session$setInputs(confirm_delete=2)
      stopifnot(nrow(rv$data)==2L)
    })
  })
  check("Shiny method module invalidates results when settings change", {
    shiny::testServer(forecasting_method_server,args=list(method="moving_average",data_bundle=shiny::reactive({
      list(y=c(10,20,30,20,40,30),name="Demand",labels=as.character(1:7),revision=0L)
    })),{
      session$setInputs(parameter_mode="manual",n=3,run=0)
      session$flushReact()
      session$setInputs(run=1)
      close_to(ready()$forecast,30)
      session$setInputs(n=2)
      stopifnot(is.null(ready()))
      session$setInputs(run=2)
      close_to(ready()$forecast,35)
    })
  })
  check("Seasonal ARIMA selection uses the explicit cycle and includes seasonal orders", {
    data <- 100 + 0.5*(1:48) + rep(c(-8,2,9,-3),12) + sin(1:48)
    result <- pp_box_jenkins(data,"auto_arima",period=4,seasonal_search=TRUE,max_p=2,max_q=2)
    orders <- forecast::arimaorder(result$details$model)
    stopifnot(is.finite(result$forecast),"D" %in% names(orders),isTRUE(result$parameters$Seasonal_search))
  })
  cat(sprintf("\nMethod checks: %d passed; %d failed.\n",passed,length(failures)))
  if (length(failures)) stop(paste(failures,collapse="\n"),call.=FALSE)
})
