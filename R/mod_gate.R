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
  useShinyjs()
  
  tabPanel(title = "Gate",
           
           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               uiOutput(ns("gfp_bin_1")),
               uiOutput(ns("gfp_bin_2")),
               uiOutput(ns("gfp_bin_3")),
               actionButton(inputId = ns("add_input"), label = "Add GFP bins", icon("plus"))

             ),
             
             mainPanel(
               
               # header and text description of curation ---------------------------------
               h1("How gating works."),
               p(),
               textOutput(ns("bin_ranges"))

             )))}

    
#' gate Server Functions
#'
#' @noRd 
#' @importFrom shinyjs show hide useShinyjs
mod_gate_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if (is.null(r$lower_limit_gfp)) {shinyjs::hide(id = "add_input")}
      else {shinyjs::show(id = "add_input")}
      })
    
    
    observe({
      if (input$add_input == 1) {
      output$gfp_bin_1 <- renderUI(numericRangeInput(ns("gfp_range_1"), label = "First GFP bin", value = c(signif(r$lower_limit_gfp, digits = 3),100)))
      }
      if (input$add_input == 2) {
        output$gfp_bin_2 <- renderUI(numericRangeInput(ns("gfp_range_2"), label = "Second GFP bin", value = c(101,350)))
      }
      if (input$add_input == 3) {
        output$gfp_bin_3 <- renderUI(numericRangeInput(ns("gfp_range_3"), label = "Third GFP bin", value = c(351,1000)))
        shinyjs::hide(id = "add_input")
      }
    }) |> bindEvent(input$add_input)
    
    
    output$bin_ranges <- renderText({
      if (is.null(input$gfp_range_1)) return(NULL)
        else if (!is.null(input$gfp_range_1) & is.null(input$gfp_range_2)) {
          paste0("Your first bin ranges from ", input$gfp_range_1[1], " to ", input$gfp_range_1[2], ".")
        }
      else if (!is.null(input$gfp_range_2) & is.null(input$gfp_range_3)) {
        paste0("Your first bin ranges from ", input$gfp_range_1[1], " to ", input$gfp_range_1[2], ".", " Your second bin ranges from", input$gfp_range_2[1], " to ", input$gfp_range_2[2])}
      else if (!is.null(input$gfp_range_3)) {
        paste0("Your first bin ranges from ", input$gfp_range_1[1], " to ", input$gfp_range_1[2], ".", " Your second bin ranges from", input$gfp_range_2[1], " to ", input$gfp_range_2[2],". ", "Your third bin ranges from ", input$gfp_range_3[1], " to ", input$gfp_range_3[2]," ." )
      }
      })
 
    
    
  })
}
    
## To be copied in the UI
# mod_gate_ui("gate_1")
    
## To be copied in the server
# mod_gate_server("gate_1")
