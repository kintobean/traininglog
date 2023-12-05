modSetupUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      h1("Setup")
    )
  )
}

modSetupServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
