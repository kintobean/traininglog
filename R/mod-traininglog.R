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

    observeEvent(rv_table(), {
      t <- rv_table()
      if (nrow(t) > 0) {
        for (i in 1:nrow(t)) {
          t$Exercise[i] <- as.character(selectInput(paste0("sel", i),
                                                        "",
                                                        choices = exerciseList,
                                                        width = "100px"))
        }
      }
      rv_table(t)
    })

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

    js <- c(
      "table.rows().every(function(i, tab, row) {",
      "  var $this = $(this.node());",
      "  $this.attr('id', this.data()[0]);",
      "  $this.addClass('shiny-input-container');",
      "});",
      "Shiny.unbindAll(table.table().node());",
      "Shiny.bindAll(table.table().node());"
    )


    output$workoutTable <- DT::renderDataTable({
      datatable(
        rv_table(),
        escape = FALSE,
        editable = TRUE,
        selection = 'multiple',
        rownames = FALSE,
        width = '80%',
        callback = JS(js),
        options = list(
          dom = 't'
        )
      )
    })

  })
}
