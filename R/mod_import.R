#' my_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      tagList(
        fileInput(inputId = ns("filename"),
                  accept = ".fcs",
                  label = "Select FCS file"),
        actionButton(ns("Submit"), "Submit"),


      )),
    mainPanel(
      h1("Hello, this is my first {golem}"),
      tableOutput(ns("files")),
      textOutput(ns("datasets")),
      uiOutput(ns("your_datasets")),
      tableOutput(ns("individual_FCS"))

    ))

}

#' my_module Server Functions
#'
#' @noRd
mod_import_server <- function(id){
  options(shiny.maxRequestSize = 60 * 1024^2)
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #output$files <- renderTable({input$filename})

    #datasets is now a reactive expression
    datasets <- reactive({n_datasets(input$filename$datapath)})



    #we can call the reactive expression created above using datasets()
    observeEvent(input$Submit,
                 {output$datasets <- renderText({
                   glue::glue("Your FCS file contains {datasets()} datasets.")})
                 output$files <- renderTable({input$filename})

                 # walk(seq_len(datasets()), \(x) {
                 #   fr <- read.FCS(input$filename$datapath,
                 #                  dataset = x,
                 #                  transformation = FALSE,
                 #                  truncate_max_range = FALSE,
                 #                  alter.names = TRUE,
                 #                  emptyValue = FALSE)
                 #   #message(paste("Write file #", x, "well", fr@description$`$WELLID`))
                 #   # write the individual flowframe objects to individual FCS files
                 #   write.FCS(fr, fs::path("fcs_input", glue::glue("dataset_{fr@description$`$WELLID`}.fcs")))
                 # })
                 # output$your_datasets <- renderUI({h2("Here are your datasets!")})
                 # output$individual_FCS <- renderTable({fs::dir_ls("fcs_input",
                 #                                                  glob = "*.fcs")})


                 })



    # fs <- read.flowSet(fs::dir_ls("fcs_input", glob = "*.fcs"),
    #                    truncate_max_range = FALSE,
    #                    alter.names = TRUE,
    #                    transformation = FALSE)
  })
}



#' @description Count how many datasets are in a FCS file
#'
#' @param filename
#'
#' @noRd
#'
#' @importFrom purrr walk
#' @import flowCore

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
