#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny DT dplyr
#' @noRd

app_server <- function(input, output, session) {

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
    rv_table(editData(rv_table(), input$workoutTable_cell_edit, 'workoutTable', rownames = FALSE))
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

    t <- rv_table()

    rv_table(read_program(input$importButton$datapath))

  })

  js <- c(
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  if(key == 13 && targetName == 'body'){",
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    "table.on('keydown', function(e){",
    "  if(e.target.localName == 'input' && [9,13,37,38,39,40].indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  if(type == 'keydown' && targetName == 'input'){",
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
  )


  output$workoutTable <- DT::renderDataTable({
    datatable(
      rv_table(),
      editable = TRUE,
      selection = 'multiple',
      rownames = FALSE,
      width = '80%',
      callback = JS(js),
      options = list(
        keys = TRUE
      ),
      extensions = "KeyTable"
    )
  })

}
