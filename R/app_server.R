#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny DT dplyr
#' @noRd

app_server <- function(input, output, session) {

  modSetupServer("setup")
  modTrainingLogServer("log")
  modAnalyticsServer("analytics")

}
