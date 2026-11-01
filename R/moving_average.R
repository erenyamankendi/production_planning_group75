# Compatibility entry points use the same calculation and presentation module.
moving_average_ui <- function(id) forecasting_method_ui(id, "moving_average")
moving_average_server <- function(id, demand_data, demand_name) {
  bundle <- shiny::reactive({
    y <- pp_series(demand_data())
    list(y = y, name = demand_name(), labels = as.character(seq_len(length(y) + 1L)), revision = 0L)
  })
  forecasting_method_server(id, "moving_average", bundle)
}
