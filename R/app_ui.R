#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import golem shiny bs4Dash DT
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(
        title = dashboardBrand(
          title = "Training Log",
        )
      ),
      sidebar = dashboardSidebar(
        disable = TRUE
      ),
      body = dashboardBody(
        DT::dataTableOutput('workoutTable', width = '85%'),
        actionButton("addButton", "Add row", icon = icon('plus')),
        actionButton("dupeButton", "Duplicate selected row(s)", icon = icon('plus')),
        actionButton("delButton", "Remove selected row(s)", icon = icon('minus')),
        fileInput("importButton", "Import program")
      ),
      controlbar = dashboardControlbar(
        bookmarkButton()
      ),
      title = "Training Log"
    )
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
