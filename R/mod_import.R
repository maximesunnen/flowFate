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


  # Defining a tabPanel layout ----------------------------------------------
  tabPanel(title = "Import",
           # Defining a sidebarlayout in the import tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               tagList(
                 # File input container in the sidebar ------------------------------------
                 fileInput(inputId = ns("filename"),
                           accept = ".fcs",
                           label = "Select FCS file"),

                 # Submit button to start the import --------------------------------------
                 actionButton(ns("Submit"), "Submit"))),

             mainPanel(
               h1("Welcome to flowFate."),
               import_text,

               # Table showing the imported file -----------------------------------------
               tableOutput(ns("files")),

               # Text indicating the number of datasets ----------------------------------
               textOutput(ns("datasets")),

               # Header over ind. FCS but printed only after submit ----------------------
               uiOutput(ns("your_datasets")),

               # Table showing individual FCS (interactive because DT) -------------------
               DTOutput(ns("individual_FCS")),

               # Plot the dataset selected in the DT table above -------------------------
               plotOutput(ns("overview_SSC_FSC"))

    )))

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
#' @importFrom glue glue
#'
mod_import_server <- function(id, r){

# increasing the maximum upload size  -------------------------------------
  options(shiny.maxRequestSize = 60 * 1024^2)

  moduleServer(id, function(input, output, session){
    ns <- session$ns

# defining a reactive observer with observe() ----------------------------
    observe({

# setting progress bars to indicate computation ---------------------------
      withProgress(min = 1, max = 10, expr = {

        setProgress(message = 'Calculation in progress',
                    detail = 'loading data...',
                    value = 3)

        # computing the number of datasets ----------------------------------------
        nb_ds <- n_datasets(input$filename$datapath)

        setProgress(message = 'Calculation in progress',
                    detail = 'counting datasets',
                    value = 5)

        # printing text indicating the number of datasets -------------------------
        output$datasets <- renderText({
          glue("Your FCS file contains {nb_ds} datasets.")})

        # render the table showing the uploaded file ------------------------------
        output$files <- renderTable({input$filename})

        # split the uploaded FCS file ---------------------------------------------
        individual_fcs <- split_1_fcs(nb_ds, input$filename$datapath)

        setProgress(message = 'Calculation in progress',
                    detail = 'splitting dataset...',
                    value = 7)

        # read the individual datasets into a flowSet -----------------------------
        fs <- read.flowSet(fs::dir_ls(individual_fcs, glob = "*.fcs"),
                           truncate_max_range = FALSE,
                           alter.names = TRUE,
                           transformation = FALSE)

        setProgress(message = 'Calculation in progress',
                    detail = 'reading individual data...',
                    value = 10)


        # add a well column to pData of flowSet -----------------------------------
        pData(fs)$well <- str_extract(pData(fs)$name, "[A-Z]\\d{2}")

        # render header of individual datasets only printed upon Submit -----------
        output$your_datasets <- renderUI({h3("Here are your datasets!")})

        # render DT with individual datasets, interactive -------------------------
        output$individual_FCS <- renderDT({pData(fs)},
                                          rownames = FALSE,
                                          class = "cell-border stripe")

        # create a gatingSet ------------------------------------------------------
        gs <- GatingSet(fs)

        # stratégie du petit R: variables to be shared across modules ---------------------
        r$nb_ds <- nb_ds
        r$gs <- gs
        r$fs <- fs

      })}) |> bindEvent(input$Submit, ignoreInit = TRUE)

    r$s <- reactive(input$individual_FCS_rows_selected)

    # overview SSC vs FSC plot to inspect data --------------------------------

    output$overview_SSC_FSC <- renderPlot({
      if (!is_null(r$s())) {
        ggcyto(r$gs[[r$s()]], aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
          geom_hex(bins = 150) +
          theme_bw()
      }
    })
    #technically not necessary since i changed the plot to the first page/module
    r$Submit <- reactive(input$Submit)
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

#' @importFrom crayon red
#' @importFrom glue glue_col

import_text <- glue::glue_col({"You can import your FCS file by clicking on the {red Browse} button on the left. Confirm your selection by clicking on the {red Submit} button. A table listing the datasets contained in your uploaded FCS file will appear. Selecting one or mutliple rows will show the SSC vs FSC plot of the selected dataset. You can always browse for a new FCS file, but you have to confirm your new selection by clicking on the {red Submit} button again."})

## To be copied in the UI
# mod_my_module_ui("my_module_1")

## To be copied in the server
# mod_my_module_server("my_module_1")
