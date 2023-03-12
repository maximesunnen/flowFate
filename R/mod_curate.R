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
               actionButton(ns("Curate"), "Start curation"),
               
               # plot SSC vs FSC for control samples -------------------------------------
               #plotOutput(ns("controls_ssc_fsc")),
               plotOutput(ns("non_debris_gate")),
               
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
      selectInput(ns("kras_channel"), 
                  "KRas channel", 
                  choices = c("",colnames(r$gs)))
    })
    
    output$MYHC_channel <- renderUI({
      req(r$gs)
      selectInput(ns("myhc_channel"),
                  "Myosin channel", 
                  choices = c("",colnames(r$gs)))
    })
    
    output$negative_control <- renderUI({
      req(r$fs)
      selectInput(ns("negative_dataset"), 
                  "Negative control", 
                  choices = c("",rownames(pData(r$fs))))
    })

    output$KRAS_control <- renderUI({
      req(r$fs)
      selectInput(ns("kras_dataset"),
                  "Positive control (KRAS)",
                  choices = c("",rownames(pData(r$fs))))
    })

    output$MYHC_control <- renderUI({
      req(r$fs)
      selectInput(ns("myhc_dataset"),
                  "Positive control (MYHC)",
                  choices = c("",rownames(pData(r$fs))))
    })

    # SSC vs FSC plot of control samples --------------------------------------
    ## get indices of the datasets selected
    control_indices <- reactive(c(input$myhc_dataset, 
                                  input$kras_dataset,
                                  input$negative_dataset))
    
    # output$controls_ssc_fsc <- renderPlot({
    #   req(r$gs)
    #   req(input$myhc_dataset, input$kras_dataset, input$negative_dataset)
    #   ggcyto(r$gs[[control_indices()]], aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
    #     geom_hex(bins = 150) +
    #     theme_bw()
    # })
    observe({
    output$non_debris_gate <- renderPlot({
      req(r$gs)
      req(input$myhc_dataset, input$kras_dataset, input$negative_dataset)
      gate <- exclude_debris()
      ggcyto(r$gs[[control_indices()]], aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
        geom_hex(bins = 150) +
        theme_bw() +
        geom_gate(gate) +
        geom_stats()
    })
    }) %>% bindEvent(input$Curate, ignoreInit = TRUE)
        })
}

curate_text <- glue("By curation we understand two essential steps. First, we want to focus our analysis on intact cells and not debris. We therefore need to set a gate that excludes cellular debris, which normally clusters in the lower left corner in a SSC vs FSC plot. Second, we have to define intensity thresholds in our fluorescent channels below which we cannot distinguish between a real signal and autofluorescence/background noise. We will define both the non-debris gate and the threshold using our controls.")

exclude_debris <- function() {
  pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),
                    ncol = 2,
                    nrow = 5)
  colnames(pgn_cut) <- c("SSC.HLin","FSC.HLin")
  polygonGate(filterId = "NonDebris", .gate = pgn_cut)
}

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
