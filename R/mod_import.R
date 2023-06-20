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
#' @import shinyFiles
mod_import_ui <- function(id){
  ns <- NS(id)

  # Defining a tabPanel layout ----------------------------------------------
  tabPanel(title = "Import",
           # Defining a sidebarlayout in the import tab ------------------------------
           sidebarLayout(
             # Defining the sidebarPanel
             sidebarPanel(
               tagList(
                 # Radio buttons to define the upload "type"
                 p("Select the data type you want to upload"),
                 radioButtons(ns("upload.type"), label = NULL, choiceValues = c("merged", "folder", "demo"), choiceNames = c("Single, merged FCS file", "Folder with individual FCS files", "Demo data"))), hr(),
               # File input container in the sidebar ------------------------------------
               div(id = ns("div_merge"),
                   p("Select merged FCS file"),
                   fileInput(inputId = ns("filename"), accept = ".fcs", label = NULL),hr()),
               # Folder input container in the sidebar
               div(id = ns("div_folder"),
                   p("Select a folder with FCS files"),
                   shinyDirButton(ns("directory"), label='Browse...', title = 'Please select a folder'),
                   verbatimTextOutput(ns("directorypath"))),
               # Demo upload container in the sidebar
               div(id = ns("div_demo"),
                   actionButton(ns("demo_fs"), label = "Import demo data")),
               # Submit button to start the import --------------------------------------
               actionButton(ns("submit"), "Submit", class = "btn-primary"),),

             # Defining the mainPanel
             mainPanel(
               h1(strong("Welcome to FlowFate.")),
               p("To start your analysis, import your FCS file by clicking on the ", strong("Browse"),
                 " button (on the left), then select your file. Confirm your selection by clicking on the ",
                 span("Submit", style = "color:#008cba; font-weight:bold"),
                 " button that appears after file selection. A description of your uploaded file will be displayed. To upload a new FCS file, click ", strong("Browse"), ", select the correct file and confirm again.", br(), br(), "Once the correct file has been uploaded, you have two options:", br(), br(),
                 "- switch to the ", strong("'Explore'"), " tab in the menu bar and explore your data", br(), br(),
                 "- switch to the ", strong("'Curate'"), " tab in the menu bar and start curating your data",
                 style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px"), br(),

               # Table output showing the datasets(individual) FCS files
               DTOutput(ns("individual_fcs")), br(),
               # Text output indicating the number of datasets
               textOutput(ns("datasets"))
           )))}

#' my_module Server Functions
#'
#' @noRd
#'
#' @importFrom DT renderDT
#' @importFrom flowCore read.flowSet
#' @importFrom flowWorkspace GatingSet
#' @importFrom stringr str_extract
#' @importFrom methods as
#'
mod_import_server <- function(id, r = NULL){
# increasing the maximum upload size  -------------------------------------
  options(shiny.maxRequestSize = 60 * 1024^2)

  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Defining the roots argument of the shinyDirChoose function below
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())

    # Server function to handle shinyFiles input
    shinyDirChoose(input, "directory", roots = volumes)

    # Defining the output showing the directory of the selected folder in the sidebar
    output$directorypath <- renderPrint({
      if (is.integer(input$directory)) cat("No directory selected")
          else {parseDirPath(volumes, input$directory)}
      })

    # demo_fcs: wrapped inside a large observer
    observe({
      # file name: a file integrated into the package (needs to be reduced to a few datasets only to reduce size)
      demo_filename <- system.file("demo_data_FCS3.0", package = "flowFate")
      withProgress(message = "Reading flowSet...",
                   demo_fs <- read.flowSet(path = demo_filename,
                                           truncate_max_range = FALSE,
                                           alter.names = TRUE,
                                           transformation = FALSE,
                                           emptyValue = FALSE))

      output$individual_FCS <- renderDT({pData(demo_fs)}, rownames = FALSE, class = "cell-border stripe")

      # strategie du petit r: we need to use demo_fs and GatingSet(demo_fs) in other modules
      r$fs <- demo_fs
      r$gs <- GatingSet(demo_fs)
      r$flowSet_pData <- pData(demo_fs)
    }) |> bindEvent(input$demo_fs)

    # user-submitted file
    ## hide the submit button when no file has been uploaded (input$filename only exists when a file is selected by the user)
    observe({
      if (input$upload.type == "merged" & is.null(input$filename)) shinyjs::hide("submit")
      else if (input$upload.type == "folder" & is.integer(input$directory)) shinyjs::hide("submit")
      else shinyjs::show("submit")
    })

    # make a reactive expression filename() evaluating to the name of the file uploaded by the user
    filename <- reactive({
      if (input$upload.type == "merged") {
        req(input$filename)
        input$filename
      }
      else if (input$upload.type == "folder")
      {
        parseDirPath(volumes, input$directory)
      }
      }) |> bindEvent(input$submit)

    # make a reactive expression nb_ds() [for "number of datadaset"] evaluating the number of datasets in the file uploaded by the user
    nb_ds <- reactive({
      req(input$filename$datapath)
      withProgress(message = "Couting datasets",
      n_datasets(input$filename$datapath))
      }) |> bindEvent(input$submit)

    # make a reactive expression individual_fcs(), which writes individual datasets to individual FCS files [see split_1_fcs custom function]
    ## the name of these datasets also contains the WELLID keyword
    individual_fcs <- reactive({
      withProgress(message = "Splitting datasets...",
      split_1_fcs(nb_ds(), input$filename$datapath))
    }) |> bindEvent(input$submit)

    # make a reactive expression fs() [for "flowSet"], which reads the individual fcs files into a single flowSet
    fs <- reactive({
      if (input$upload.type == "merged") {
      withProgress(message = "Reading datasets...",
      read.flowSet(fs::dir_ls(individual_fcs(), glob = "*.fcs"), truncate_max_range = FALSE, alter.names = TRUE, transformation = FALSE))
      }
      else if (input$upload.type == "folder") {
        withProgress(message = "Reading datasets...", {
          read.flowSet(path = filename(), truncate_max_range = FALSE, alter.names = TRUE, transformation = FALSE, emptyValue = FALSE)
        # pData(a)$name <- paste0("dataset_", seq_len(length(a)))
        # sampleNames(a) <- paste0("dataset_", seq_len(length(a)))})
      })}
    }) |> bindEvent(input$submit)

    # make a reactive expression flowSet_pData() [for "flowSet phenotypic data"], which corresponds to the name of the individual datasets
    flowSet_pData <- reactive({pData(fs())}) |> bindEvent(input$submit)
    ## we need this variable in other modules, so we need to use the strategie du petit r to share it across modules
    ## since I need to read a reactive value (flowSet_pData()), I need to wrap it into reactive. Not sure if r$flowSet_pData then needs to be called as a reactive expression too, and if this does not potentially lead to unexpected reactivity that will be hard to debug

    # make a reactive expression gs() [for "gatingSet"], which creates a gatingSet from the flowSet
    gs <- reactive({GatingSet(fs())}) |> bindEvent(input$submit)

    # individual_fcs output: individual FCS files (datasets) displayed as a table
    output$individual_fcs <- renderDT({r$flowSet_pData}, rownames = FALSE)

    # datasets output: text indicating the number of datasets contained inside the fcs file uploaded by the user
    output$datasets <- renderText({paste0("Your FCS file contains ", nb_ds(), " dataset(s).")})

    # strategie du petit R: all variables to be shared across modules ---------------------
    observe({
      r$flowSet_pData <- flowSet_pData()
      r$gs <- GatingSet(fs())
      r$fs <- fs()
      r$nb_ds <- nb_ds()
    })

    observe({
      if (input$upload.type == "merged") {
        shinyjs::hide("div_folder")
        shinyjs::hide("div_demo")
        shinyjs::show("div_merge")
      }
      else if (input$upload.type == "folder") {
        shinyjs::hide("div_merge")
        shinyjs::hide("div_demo")
        shinyjs::show("div_folder")
      }
      else {
        shinyjs::hide("div_merge")
        shinyjs::hide("div_folder")
        shinyjs::show("div_demo")
        shinyjs::hide("submit")
      }
    })

  })}

#' @description Count how many datasets are in a FCS file
#'
#' @param filename
#'
#' @noRd
#'
#' @return integer
n_datasets <- function(filename) {
  # Adapted code from https://github.com/RGLab/flowCore/blob/ba3b6ffed5310c1c0618487ab163c0142d8cab8f/R/IO.R

  # the keyword $NEXTDATA contains either a zero when there are no next data
  # or a positive integer for the next dataset
  counter <- 0
  nextdata <- 1
  while (nextdata != 0) {
    counter <- counter + 1
    nextdata <- read.FCSheader(filename, keyword = "$NEXTDATA", emptyValue = FALSE, dataset = counter)
    nextdata <- as.integer(nextdata[[1]]["$NEXTDATA"])
  }
  counter
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
    write.FCS(fr, path(path_dir(input_file), "fcs_input", paste0("dataset_", fr@description$`$WELLID`, ".fcs")))
  })
  path(path_dir(input_file), "fcs_input")
}

## To be copied in the UI
# mod_my_module_ui("my_module_1")

## To be copied in the server
# mod_my_module_server("my_module_1")
