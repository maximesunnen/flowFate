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
    tabPanel(title = "Curate",
             sidebarLayout(
      sidebarPanel(
        uiOutput(ns("KRAS_selection")),
        uiOutput(ns("MYHC_channel"))
        ),
      
    mainPanel(
      
      h1("How curation works."),
      
      p("By curation we understand two essential steps. First, we want to focus our analysis on intact cells and not debris. We therefore need to set a gate that excludes cellular debris, which normally clusters in the lower left corner in a SSC vs FSC plot. Second, we have to define intensity thresholds in our fluorescent channels below which we cannot distinguish between a real signal and autofluorescence/background noise. We will define both the non-debris gate and the threshold using our controls."),
      actionButton("Curate", "Start curation"),
      plotOutput("ssc_fsc"),
      textOutput(ns("cur_ds")),
      textOutput(ns("test"))
    )))}

#' curate Server Functions
#'
#' @noRd
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$KRAS_selection <- renderUI({
      selectInput("kras_channel", "KRas channel", choices = colnames(r$gs))
    })
    
    output$MYHC_channel <- renderUI({
      selectInput("myhc_channel", "Myosin channel", choices = colnames(r$gs))
    })
    
    output$test <- renderText({glue::glue("You selected dataset number {r$s()}")})
        })
  }

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
