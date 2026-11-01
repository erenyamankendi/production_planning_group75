# End-to-end server wiring and chronological holdout checks.
local({
  arg <- sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE)[1])
  app <- dirname(dirname(normalizePath(arg)))
  version <- paste(R.version$major,strsplit(R.version$minor,".",fixed=TRUE)[[1]][1],sep=".")
  .libPaths(c(file.path(dirname(app),".R-library"),file.path(Sys.getenv("USERPROFILE"),"AppData","Local","R","win-library",version),.libPaths()))
  for (file in list.files(file.path(app,"R"),pattern="[.]R$",full.names=TRUE)) sys.source(file,envir=environment())
  fixture <- file.path(app,"tests","fixtures","seasonal_demand.csv")
  data <- read.csv(fixture)
  parent <- function(input,output,session) {
    dataset <- data_management_server("data")
    methods <- forecasting_server("forecasting",dataset)
  }
  shiny::testServer(parent,{
    session$setInputs(`data-file`=data.frame(name="seasonal_demand.csv",size=file.info(fixture)$size,type="text/csv",datapath=fixture),
      `data-target`="Demand",`data-period_column`="Period",`data-time_unit`="period",`data-separator`="auto",`data-decimal`=".")
    session$flushReact()
    stopifnot(length(dataset()$y)==48L)
    for (id in pp_catalog()$id) {
      values <- list(n=3,weights="0.5, 0.3, 0.2",parameter_mode="manual",alpha=.2,beta=.1,gamma=.1,
        init_mode="regression",init_count=NA_real_,init_level=NA_real_,init_slope=NA_real_,period=4,start_phase=1,
        season_type="additive",p=1,d=1,q=1,include_constant=TRUE,seasonal_search=FALSE,max_p=2,max_q=2,search_mode="thorough",run=0)
      names(values) <- paste0("forecasting-method_",id,"-",names(values))
      do.call(session$setInputs,values)
      do.call(session$setInputs,setNames(list(1),paste0("forecasting-method_",id,"-run")))
      result <- methods[[id]]$result()
      stopifnot(!is.null(result),is.finite(result$forecast),result$metrics$count>0)
    }
    session$setInputs(`data-table_cell_edit`=list(row=1L,col=1L,value="94"))
    stopifnot(all(vapply(methods,function(module)is.null(module$result()),logical(1))))
    session$setInputs(`data-table_cell_edit`=list(row=1L,col=1L,value=""))
    stopifnot(!is.null(tryCatch({dataset();NULL},error=identity)))
    session$setInputs(`data-table_cell_edit`=list(row=1L,col=1L,value="93"))
    stopifnot(length(dataset()$y)==48L)
  })
  cat("PASS: all 12 modules share uploaded data, calculate, invalidate together and block incomplete data.\n")
  y <- data$Demand
  cutoff <- length(y)-4L
  base <- y[seq_len(cutoff)]
  selected_n <- pp_moving_average(base,mode="auto")$parameters$N
  auto_model <- pp_box_jenkins(base,"auto_arima",max_p=2,max_q=2)$details$model
  rows <- lapply(pp_catalog()$id,function(id) {
    predictions <- vapply(seq.int(cutoff,length(y)-1L),function(origin) {
      history <- y[seq_len(origin)]
      if (id=="auto_arima") return(as.numeric(forecast::forecast(forecast::Arima(history,model=auto_model),h=1)$mean[1]))
      params <- list(period=4,n=if(id=="moving_average")selected_n else 3L)
      pp_fit(id,history,params)$forecast
    },numeric(1))
    actual <- tail(y,4)
    stopifnot(all(is.finite(predictions)))
    errors <- actual-predictions
    data.frame(Method=id,Test_observations=4L,MAD=mean(abs(errors)),MSE=mean(errors^2),MAPE=100*mean(abs(errors/actual)))
  })
  report <- do.call(rbind,rows)
  write.csv(report,file.path(app,"tests","holdout_results.csv"),row.names=FALSE)
  cat("PASS: chronological one-step holdout predictions for all 12 methods; automatic choices fixed before holdout.\n")
  print(report,row.names=FALSE)
})
