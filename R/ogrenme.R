learning_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(shiny::sliderInput(ns("rate"), "Learning rate", 0.6, 0.99, 0.8),
                       shiny::p("The first unit takes 100 time units.")),
    shiny::mainPanel(shiny::h3("Learning Curve"), shiny::plotOutput(ns("curve")))
  )
}
learning_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$curve <- shiny::renderPlot({
      shiny::req(input$rate)
      units <- seq_len(50L)
      exponent <- log(input$rate) / log(2)
      graphics::plot(units, 100 * units^exponent, type = "l", lwd = 2,
                     xlab = "Cumulative units", ylab = "Time per unit", main = "Unit learning curve")
    })
  })
}
