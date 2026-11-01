# Shiny automatically loads the module scripts in R/ before evaluating this file.
local({
  version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
  candidates <- c(file.path(getwd(), "..", ".R-library"), file.path(getwd(), ".R-library"),
                  file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", version),
                  file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", version))
  .libPaths(c(candidates[dir.exists(candidates)], .libPaths()))
})
required <- c("shiny", "DT", "readxl", "ggplot2", "forecast")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop(paste("Required R packages are missing:", paste(missing, collapse = ", "),
                                ". Run setup.R from the workspace, then restart the app."), call. = FALSE)
library(shiny)

ui <- navbarPage("Production Planning & Control",
  tabPanel("Data Management", data_management_ui("data")),
  tabPanel("Forecasting", forecasting_ui("forecasting")),
  tabPanel("Learning Curve", learning_ui("learning")),
  footer = tags$div(style = "padding: 12px 16px; color: #555;",
    "EMU 341 - Group 75 | Forecast errors = actual - forecast. Historical fitting/selection accuracy is labelled separately from independent test accuracy.")
)

server <- function(input, output, session) {
  dataset <- data_management_server("data")
  forecasting_server("forecasting", data_bundle = dataset)
  learning_server("learning")
}

shinyApp(ui, server)
