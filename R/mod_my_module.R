#' my_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_my_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Hello, this is my first {golem}"),

    fileInput(inputId = ns("filename"),
              accept = ".fcs",
              label = "Select FCS file"),

    tableOutput(ns("files")),

    textOutput(ns("text")),

    textOutput(ns("datasets"))
  )
}

#' my_module Server Functions
#'
#' @noRd
mod_my_module_server <- function(id){
  options(shiny.maxRequestSize = 60 * 1024^2)
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$files <- renderTable({input$filename})

    datasets <- reactive({n_datasets(input$filename$datapath)})

    output$text <- renderText({"Hello Aurélien"})

    output$datasets <- renderText({datasets()})
  })
}



n_datasets <- function(filename) {
  # Adapted code from https://github.com/RGLab/flowCore/blob/ba3b6ffed5310c1c0618487ab163c0142d8cab8f/R/IO.R
  con <- file(filename, open = "rb")

  offsets <- flowCore:::readFCSheader(con)
  offsets <- matrix(offsets, nrow = 1, dimnames = list(NULL, names(offsets)))
  txt <- flowCore:::readFCStext(con, offsets[1, ],emptyValue = FALSE)

  addOff <- 0

  if ("$NEXTDATA" %in% names(txt)) {
    nd <- as.numeric(txt[["$NEXTDATA"]])
  } else
    nd <- 0

  txt.list <- list(txt)
  i <- 1
  while (nd != 0) {
    i <- i + 1
    addOff <- addOff + nd
    offsets <- rbind(offsets, flowCore:::readFCSheader(con, addOff))
    this.txt <- flowCore:::readFCStext(con, offsets[nrow(offsets),], emptyValue = FALSE)
    nd <- as.numeric(this.txt[["$NEXTDATA"]])
    txt.list[[i]] <- this.txt
  }
  message("found", length(txt.list), "nb datasets")
  length(txt.list)
}






## To be copied in the UI
# mod_my_module_ui("my_module_1")

## To be copied in the server
# mod_my_module_server("my_module_1")
