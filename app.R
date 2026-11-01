library(shiny)

ui <- navbarPage("Production Planning",
  tabPanel("Learning Curve", learning_ui("learning")),
  navbarMenu("Forecasting",
    "Stationary series",
    tabPanel("Moving Average", moving_average_ui("moving_average")),
    tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_ma")),
    tabPanel("Simple Exponential Smoothing", exponential_smoothing_ui("ses")),
    tabPanel("Seasonal Factors (quick and dirty)", seasonal_factors_ui("seasonal_factors")),
    "----",
    "Trend series",
    tabPanel("Holt's Method", holt_ui("holt")),
    "----",
    "Seasonal series",
    tabPanel("Seasonal Decomposition (moving averages)",
             seasonal_decomposition_ui("decomposition")),
    tabPanel("Winters's Method", winters_ui("winters")),
    "----",
    "Box-Jenkins",
    tabPanel("AR", box_jenkins_ui("bj_ar", "AR")),
    tabPanel("MA", box_jenkins_ui("bj_ma", "MA")),
    tabPanel("ARMA", box_jenkins_ui("bj_arma", "ARMA")),
    tabPanel("ARIMA", box_jenkins_ui("bj_arima", "ARIMA"))
  )
)

server <- function(input, output, session) {
  learning_server("learning")
  moving_average_server("moving_average")
  weighted_moving_average_server("weighted_ma")
  exponential_smoothing_server("ses")
  seasonal_factors_server("seasonal_factors")
  holt_server("holt")
  seasonal_decomposition_server("decomposition")
  winters_server("winters")
  box_jenkins_server("bj_ar", "AR")
  box_jenkins_server("bj_ma", "MA")
  box_jenkins_server("bj_arma", "ARMA")
  box_jenkins_server("bj_arima", "ARIMA")
}

shinyApp(ui, server)
