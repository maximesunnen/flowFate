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
                
               actionButton(inputId = ns("add_input"), label = "Add GFP bins", icon("plus")),

               # illustration how conditionalPanel works
               conditionalPanel(
                 # still a JS expression
                 condition = "input.add_input == 1", 
                 #make sure that input.<input> reacts to (and only to) to input from this module
                 ns = ns, 
                 #what should happen if condition is met
                 checkboxInput(ns("headsonly"), "This text should....")),
               
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
    
    ##SETTING UP THE GFP-BINS
    
    # hide "Add GFP bins" button when r$lower_limit_gfp is NULL, show it when it is not NULL
    observe({
      if (is.null(r$lower_limit_gfp)) {shinyjs::hide(id = "add_input")}
      else {shinyjs::show(id = "add_input")}
    })
    
    # render uiOutputs sequentially as the user clicks the "add_input" button
    observe({
      if (is.null(input$add_input)) return(NULL)
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
    
    
    ## SET UP A GATE ACCORDING TO THE USER-DEFINED BIN RANGE
    # 
    observe({
      if(is.null(input$add_input)) return(NULL)
      gate_limits <- list(low = list(c(input$gfp_range_1[1], input$gfp_range_1[2])),
                          medium = list(c(input$gfp_range_2[1], input$gfp_range_2[2])),
                          high = list(c(input$gfp_range_3[1], input$gfp_range_3[2])))
      print(gate_limits)
      print(r$kras_channel())
      
      if(!is.null(input$gfp_range_1) && !is.null(input$gfp_range_2) && !is.null(input$gfp_range_3)) {
        gates <- lapply(gate_limits, function(x) {
          names(x) <- r$kras_channel()
          rectangleGate(x)
      })
        print(gates)
      }
    })
    
  })
}



#' #' @importFrom stringr str_to_upper
#' create_bin_button <- function (channel, bin_number, label, ns, lower_limit) {
#'   if (channel == "gfp") {
#'     if (bin_number == 1) {
#'     conditionalPanel(condition = paste0("input.add_input >= ", bin_number),
#'                      ns = ns, 
#'                      numericRangeInput(ns(paste0(channel, "_range_", bin_number)), 
#'                                        label = paste0(label," ", str_to_upper(channel), " ", "bin"),
#'                                        value = c(signif(lower_limit, digits = 3),100)))
#'     }
#'     else if (bin_number == 2) {
#'       conditionalPanel(condition = paste0("input.add_input >= ", bin_number),
#'                        ns = ns, 
#'                        numericRangeInput(ns(paste0(channel, "_range_", bin_number)), 
#'                                          label = paste0(label," ", str_to_upper(channel), " ", "bin"),
#'                                          value = c(101,350)))
#'     }
#'     else if (bin_number == 3) {
#'       conditionalPanel(condition = paste0("input.add_input >= ", bin_number),
#'                        ns = ns, 
#'                        numericRangeInput(ns(paste0(channel, "_range_", bin_number)), 
#'                                          label = paste0(label," ", str_to_upper(channel), " ", "bin"),
#'                                          value = c(351,1000)))
#'     }
#'   }
#' }

## To be copied in the UI
# mod_gate_ui("gate_1")
    
## To be copied in the server
# mod_gate_server("gate_1")
