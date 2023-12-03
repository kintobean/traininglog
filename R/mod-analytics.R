modAnalyticsUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      h1("Analytics")
    )
  )
}

modAnalyticsServer <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}
