#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import golem shiny bslib DT
#' @noRd
app_ui <- function(request) {

  page_navbar(
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      heading_font = font_google("Cabin"),
      base_font = font_google("Raleway")
    ),
    nav_panel("Setup", modSetupUI("setup")),
    nav_panel("Training Log", modTrainingLogUI("log")),
    nav_panel("Analytics", modAnalyticsUI("analytics"))
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "traininglog"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
