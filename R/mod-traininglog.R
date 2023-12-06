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
      "1-Arm Cable Row",
      "Bench Press",
      "Bicep Curls",
      "Chest Fly",
      "Conventional Deadlift",
      "DB Row",
      "DB SLDL",
      "Facepulls",
      "Front Squat",
      "Hammer Curls",
      "Highbar Squat",
      "Incline DB Press",
      "Lat Pulldown",
      "Lat Pullover",
      "Lateral Raises",
      "Lowbar Squat",
      "Overhead Press",
      "Pause Bench",
      "Pause Squat",
      "Pullup",
      "Seated DB Press",
      "Sumo Deadlift",
      "TnG Bench",
      "Tricep Extensions"
    )

    blankrow <- data.frame(
      Exercise = character(),
      Exercise2 = character(),
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

      optionsString <- '<option value="" selected></option>\n'
      for (exercise in exerciseList) {
        optionsString <- paste0(optionsString, sprintf('<option value="%s">%s</option>\n', exercise, exercise))
      }

      matching_inputs <- grep("^single_select[0-9]+$", names(input), value = TRUE)

      # Create a list of input values
      input_values_list <- lapply(matching_inputs, function(matching_input) {
        return(input[[matching_input]])
      })

      # browser()

      t <- rv_table() %>%
        add_row(
          Exercise = '',
          Exercise2 = paste0('<select id="log-single_select',
                             input$addButton,
                             '" style="width: 100%;">',
                             optionsString,
                             '</select>'),
          Sets = '',
          Reps = '',
          Intensity = '',
          Load = '',
          RPE = '',
          Notes = '')

      if (length(input_values_list) > 0) {
        for (i in length(input_values_list)){
          t$Exercise2[i] = input_values_list[[i]]
        }
      }

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

    # js <- c(
    #   "table.rows().every(function(i, tab, row) {",
    #   "  var $this = $(this.node());",
    #   "  $this.attr('id', this.data()[0]);",
    #   "  $this.addClass('shiny-input-container');",
    #   "});",
    #   "Shiny.unbindAll(table.table().node());",
    #   "Shiny.bindAll(table.table().node());"
    # )


    output$workoutTable <- DT::renderDataTable({
      datatable(
        rv_table(),
        escape = FALSE,
        editable = TRUE,
        selection = 'multiple',
        rownames = FALSE,
        width = '80%',
        options = list(
          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
        )
      )
    })

  })
}
