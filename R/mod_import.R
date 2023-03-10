#' my_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
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
      DTOutput(ns("individual_FCS"))

    ))

}

#' my_module Server Functions
#'
#' @noRd
#'
#' @importFrom DT renderDT
#' @importFrom flowCore read.flowSet
#' @importFrom flowWorkspace GatingSet
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
mod_import_server <- function(id, r){
  options(shiny.maxRequestSize = 60 * 1024^2)
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      withProgress(min = 1, max = 10, expr = {
        setProgress(message = 'Calculation in progress',
                    detail = 'loading data...',
                    value = 3)
      nb_ds <- n_datasets(input$filename$datapath)
      setProgress(message = 'Calculation in progress',
                  detail = 'counting datasets',
                  value = 5)
      output$datasets <- renderText({
      glue::glue("Your FCS file contains {nb_ds} datasets.")})
      files <- input$filename
      output$files <- renderTable({files})
      individual_fcs <- split_1_fcs(nb_ds, input$filename$datapath)
      setProgress(message = 'Calculation in progress',
                  detail = 'splitting dataset...',
                  value = 7)
      fs <- read.flowSet(fs::dir_ls(individual_fcs, glob = "*.fcs"),
                         truncate_max_range = FALSE,
                         alter.names = TRUE,
                         transformation = FALSE)
      setProgress(message = 'Calculation in progress',
                  detail = 'reading individual data...',
                  value = 10)
      pData(fs)$well <- str_extract(pData(fs)$name, "[A-Z]\\d{2}")
      output$your_datasets <- renderUI({h2("Here are your datasets!")})
      output$individual_FCS <- renderDT({pData(fs)},
                                        rownames = FALSE,
                                        class = "cell-border stripe")
      gs <- GatingSet(fs)
      r$nb_ds <- nb_ds
      r$Submit <- input$Submit
      r$gs <- gs
      })}) %>% bindEvent(input$Submit, ignoreInit = TRUE)
    
    #s <- reactive(input$individual_FCS_rows_selected)
    r$s <- reactive(input$individual_FCS_rows_selected)

  })
}



#' @description Count how many datasets are in a FCS file
#'
#' @param filename
#'
#' @noRd
#'
#' @return integer
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
  close(con)
  #message("found", length(txt.list), "nb datasets")
  length(txt.list)
}

#' @description Split multiple FCS datasets into individual fcs files
#'
#' @param dataset a flowCore dataset
#'
#' @noRd
#'
#' @importFrom purrr walk
#' @import flowCore
#' @import fs
#' @return folder path of individual fcs
#'
split_1_fcs <- function(nb, input_file) {

  if (!dir_exists(path(path_dir(input_file), "fcs_input"))) dir_create(path(path_dir(input_file), "fcs_input"))
  walk(seq_len(nb), \(x) {
    fr <- read.FCS(input_file,
                   dataset = x,
                   transformation = FALSE,
                   truncate_max_range = FALSE,
                   alter.names = TRUE,
                   emptyValue = FALSE)
    #message(paste("Write file #", x, "well", fr@description$`$WELLID`))
    # write the individual flowframe objects to individual FCS files
    write.FCS(fr, path(path_dir(input_file), "fcs_input", glue::glue("dataset_{fr@description$`$WELLID`}.fcs")))
  })
  path(path_dir(input_file), "fcs_input")
}

## To be copied in the UI
# mod_my_module_ui("my_module_1")

## To be copied in the server
# mod_my_module_server("my_module_1")
