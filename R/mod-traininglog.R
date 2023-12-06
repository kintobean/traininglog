modTrainingLogUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      h1("Training Log")
      ),
    div(
      fileInput(ns("importButton"), "Import program")
      ),
    div(
      DT::dataTableOutput(ns('workoutTable'), width = '85%')
      ),
    div(
      actionButton(ns("addButton"), "Add row", icon = icon('plus')),
      actionButton(ns("dupeButton"), "Duplicate selected row(s)", icon = icon('plus')),
      actionButton(ns("delButton"), "Remove selected row(s)", icon = icon('minus')),
    ),
    div(
      textInput(ns("mongousername"), "Username"),
      textInput(ns("mongopassword"), "Password")
    ),
    div(
      actionButton(ns("exportmongo"),"Export to Mongo")
    )
  )
}

modTrainingLogServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    exerciseList <- c(
      "Squat",
      "Bench",
      "Deadlift",
      "Overhead Press"
    )

    blankrow <- data.frame(
      Exercise = character(),
      Sets = character(),
      Reps = character(),
      Intensity = character(),
      Load = character(),
      RPE = character(),
      Notes = character()
    )

    rv_table <- reactiveVal(blankrow)

    observeEvent(input$workoutTable_cell_edit, {
      rv_table(editData(rv_table(), input$workoutTable_cell_edit, ns('workoutTable'), rownames = FALSE))
    })

    observeEvent(input$addButton, {
      t <- rv_table() %>%
        add_row(
          Exercise = '',
          Sets = '',
          Reps = '',
          Intensity = '',
          Load = '',
          RPE = '',
          Notes = '')

      rv_table(t)
    })

    observeEvent(input$dupeButton, {
      t <- rv_table()

      if (!is.null(input$workoutTable_rows_selected)) {

        t <- rbind(t, t[as.numeric(input$workoutTable_rows_selected),])
      }
      rv_table(t)

    })

    observeEvent(input$delButton, {
      t <- rv_table()

      if (!is.null(input$workoutTable_rows_selected)) {

        t <- t[-as.numeric(input$workoutTable_rows_selected),]
      }
      rv_table(t)
    })

    observeEvent(input$importButton, {

      rv_table(read_program(input$importButton$datapath))

    })

    output$workoutTable <- DT::renderDataTable({
      datatable(
        rv_table(),
        editable = TRUE,
        selection = 'multiple',
        rownames = FALSE,
        width = '80%'
      )
    })

    observeEvent(input$exportmongo, {

      options(mongodb = list(
        "host" = "cluster0.lta9lc0.mongodb.net",
        "username" = input$mongousername,
        "password" = input$mongopassword
      ))

      databaseName <- "traininglogdb"
      collectionName <- "traininglogs"

      saveData <- function(data) {
        # Connect to the database
        db <- mongolite::mongo(collection = collectionName,
                               url = sprintf(
                                 "mongodb+srv://%s:%s@%s/%s",
                                 options()$mongodb$username,
                                 options()$mongodb$password,
                                 options()$mongodb$host,
                                 databaseName
                               ),
                               options = mongolite::ssl_options(weak_cert_validation = TRUE))
        # Insert the data into the mongo collection as a data.frame
        data <- as.data.frame(t(data))
        db$insert(data)
      }

      loadData <- function() {
        # Connect to the database
        db <- mongolite::mongo(collection = collectionName,
                               url = sprintf(
                                 "mongodb+srv://%s:%s@%s/%s",
                                 options()$mongodb$username,
                                 options()$mongodb$password,
                                 options()$mongodb$host,
                                 databaseName
                               ),
                               options = mongolite::ssl_options(weak_cert_validation = TRUE))
        # Read all the entries
        data <- db$find()
        data
      }

      req(input$mongousername)
      req(input$mongopassword)

      tryExport <- function() {
        result <- tryCatch({
          saveData(rv_table())
          "success"
        }, warning = function(warning_message) {
          cat("Warning: ", warning_message$message, "\n")
          "warning"
        }, error = function(error_message) {
          cat("Error: ", error_message$message, "\n")
          "error"
        }, finally = {
          cat("done\n")
        })

        return(result)
      }

      # Example usage
      result <- tryExport()

      # Check the result
      if (result == "success") {
        cat("The operation was successful!\n")
      } else if (result == "warning") {
        cat("The operation produced a warning.\n")
      } else if (result == "error") {
        cat("The operation encountered an error.\n")
      } else {
        cat("Unknown result.\n")
      }

    })

  })
}
