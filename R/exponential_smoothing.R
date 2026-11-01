# Simple smoothing and Holt have separate entries under Forecasting.
exponential_smoothing_ui <- function(id) forecasting_method_ui(id, "ses")
exponential_smoothing_server <- function(id, demand_data, demand_name) {
  bundle <- shiny::reactive({
    y <- pp_series(demand_data())
    list(y = y, name = demand_name(), labels = as.character(seq_len(length(y) + 1L)), revision = 0L)
  })
  forecasting_method_server(id, "ses", bundle)
}
