#' gate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets numericRangeInput actionBttn
mod_gate_ui <- function(id){
  ns <- NS(id)
  tabPanel(title = "Gate",
           
           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               numericRangeInput(ns("gfp_bin_1"), label = "First GFP bin", value = c(100, 350)),
               uiOutput(ns("gfp_bin_2")),
               actionButton(inputId = ns("add_input"), icon("plus"))

             ),
             
             mainPanel(
               
               # header and text description of curation ---------------------------------
               h1("How gating works."),
               p(),

             )))}

    
#' gate Server Functions
#'
#' @noRd 
mod_gate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$add_input,
                 output$gfp_bin_2 <- renderUI(numericRangeInput(ns("test"), label = "Second GFP bin", value = c(100, 350))))
 
  })
}
    
## To be copied in the UI
# mod_gate_ui("gate_1")
    
## To be copied in the server
# mod_gate_server("gate_1")
