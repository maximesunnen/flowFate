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
                 actionButton(ns("Submit"), "Submit", class = "btn-primary"))),

             mainPanel(
               tabsetPanel(
                 tabPanel("Information", icon = icon("info"),
               h1(strong("Welcome to flowFate.")),
               p("To import your FCS file, click on the " , strong("Browse"),
                 " button on the left and select your file. Confirm your selection by clicking on the ",
                 span("Submit", style = "color:#008cba; font-weight:bold"),
                 " button. A table containing a description of the uploaded file as well as your FCS file's individual datasets appears. We've added a 'well-name' column so you can verify if you've uploaded the correct file. You can click on and select one (or multiple) rows to show the SSC vs FSC plot of the selected dataset(s).", br(), br(),
                 "To upload a new FCS file, click on the " , strong("Browse"), " button and select the correct file. Don't forget to confirm your selection again by clicking on the ", span("Submit", style = "color:#008cba; font-weight:bold"), " button.", br(), br(),
                 "Once the correct file has been uploaded, you can proceed with the curation of your data. Simply click on the ",
                 strong("Curate"), " tab at the top of the page.",
                 style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px"),
               br()),
               tabPanel("Uploaded FCS file",
               br(),
               # Table showing the imported file -----------------------------------------
               tableOutput(ns("files")),
               # Text indicating the number of datasets ----------------------------------
               textOutput(ns("datasets"))),
               
               tabPanel("Your datasets",
               br(),
               # Table showing individual FCS (interactive because DT) -------------------
               DTOutput(ns("individual_FCS")),
               br(),
               # Plot the dataset selected in the DT table above -------------------------
               plotOutput(ns("overview_SSC_FSC"))))
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
mod_import_server <- function(id, r = NULL){
# increasing the maximum upload size  -------------------------------------
  options(shiny.maxRequestSize = 60 * 1024^2)

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      if (is.null(input$filename)) shinyjs::hide("Submit")
      else shinyjs::show("Submit")
    })
    
    filename <- reactive({
      req(input$filename)
      input$filename
    }) |> bindEvent(input$Submit)
    
    nb_ds <- reactive({
      req(input$filename$datapath)
      withProgress(message = "Counting datasets...",
      n_datasets(input$filename$datapath))
      }) |> bindEvent(input$Submit)
    
    individual_fcs <- reactive({
      req(input$filename$datapath)
      withProgress(message = "Splitting datasets...",
      split_1_fcs(nb_ds(), input$filename$datapath))
    }) |> bindEvent(input$Submit)
    
    fs <- reactive({
      withProgress(message = "Reading datasets...",
      read.flowSet(fs::dir_ls(individual_fcs(), glob = "*.fcs"),
                   truncate_max_range = FALSE,
                   alter.names = TRUE,
                   transformation = FALSE))
    }) |> bindEvent(input$Submit)
    
    flowSet_pData <- reactive({pData(fs())}) |> bindEvent(input$Submit)
    
    #not sure where to put this now
    # pData(fs())$well <- str_extract(pData(fs())$name, "[A-Z]\\d{2}")
    
    gs <- reactive({
      GatingSet(fs())
      }) |> bindEvent(input$Submit)
    
    selected_rows <- reactive(input$individual_FCS_rows_selected)
    
    output$datasets <- renderText({
      #req(input$filename)
      paste0("Your FCS file contains ", nb_ds(), " datasets.")
    })
    
    output$files <- renderTable({
      req(input$Submit)
      filename()
    })
    
    output$individual_FCS <- renderDT({flowSet_pData()},
                                      rownames = FALSE,
                                      class = "cell-border stripe")

    # stratégie du petit R: variables to be shared across modules ---------------------
    
    observe({
      r$nb_ds <- nb_ds()
      r$gs <- gs()
      r$fs <- fs()
    })

    # overview SSC vs FSC plot to inspect data --------------------------------

    output$overview_SSC_FSC <- renderPlot({
      if (!is_null(selected_rows())) {
        ggcyto(gs()[[selected_rows()]], aes(x = "SSC.HLin", y = "FSC.HLin"), subset = "root") +
          geom_hex(bins = 150) +
          theme_bw()
      }
    })

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
#' @rawNamespace import(flowCore, except = show)
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
