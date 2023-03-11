#' curate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggcyto
#' @import ggplot2
#' @import magrittr

mod_curate_ui <- function(id){
  ns <- NS(id)
  
  # Defining a tabPanel layout ----------------------------------------------
  tabPanel(title = "Curate",
           
           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             sidebarPanel(

               # Input selections for used channels and control samples ------------------
               uiOutput(ns("KRAS_selection")),
               uiOutput(ns("MYHC_channel")),
               uiOutput(ns("negative_control")),
               uiOutput(ns("KRAS_control")),
               uiOutput(ns("MYHC_control"))
             ),
             
             mainPanel(

               # header and text description of curation ---------------------------------
               h1("How curation works."),
               curate_text,
               
               # Action button to start curation
               actionButton("Curate", "Start curation"),
               
               # plot SSC vs FSC for control samples -------------------------------------
               plotOutput("controls_ssc_fsc"),
               
               textOutput(ns("cur_ds")),
               textOutput(ns("test"))
             )))}

#' curate Server Functions
#'
#' @noRd
#' @importFrom purrr is_null
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # All selections from sidebar (control datasets and channels) -------------
    output$KRAS_selection <- renderUI({
      req(r$gs)
      selectInput("kras_channel", 
                  "KRas channel", 
                  choices = colnames(r$gs))
    })
    
    output$MYHC_channel <- renderUI({
      req(r$gs)
      selectInput("myhc_channel",
                  "Myosin channel", 
                  choices = colnames(r$gs))
    })
    
    output$negative_control <- renderUI({
      req(r$fs)
      selectInput("negative_dataset", 
                  "Negative control", 
                  choices = rownames(pData(r$fs)))
    })

    output$KRAS_control <- renderUI({
      req(r$fs)
      selectInput("kras_dataset",
                  "Positive control (KRAS)",
                  choices = rownames(pData(r$fs)))
    })

    output$MYHC_control <- renderUI({
      req(r$fs)
      selectInput("myhc_dataset",
                  "Positive control (MYHC)",
                  choices = rownames(pData(r$fs)))
    })

    # SSC vs FSC plot of control samples --------------------------------------
    ## get indices of the datasets selected
    control_indices <- reactive(c(input$negative_dataset, 
                                  input$kras_dataset, 
                                  input$myhc_dataset))
    
    
    output$controls_ssc_fsc <- renderPlot({
      ggcyto(r$gs[[1]], aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
        geom_hex(bins = 150) +
        theme_bw()
    })
      
    output$test <- renderText({glue::glue("You selected dataset number {r$s()}")})
        })
}




curate_text <- glue("By curation we understand two essential steps. First, we want to focus our analysis on intact cells and not debris. We therefore need to set a gate that excludes cellular debris, which normally clusters in the lower left corner in a SSC vs FSC plot. Second, we have to define intensity thresholds in our fluorescent channels below which we cannot distinguish between a real signal and autofluorescence/background noise. We will define both the non-debris gate and the threshold using our controls.")

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
