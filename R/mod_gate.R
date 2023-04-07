#' gate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets numericRangeInput
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
                actionButton(inputId = ns("Gate"), label = "Gate now")
             ),
             
             mainPanel(
               # header and text description of gating ---------------------------------
               h1("How gating works."),
               p(),
               # outputs
               textOutput(ns("test")),
               plotOutput(ns("test_plot"))
             )))}
    
#' gate Server Functions
#' @noRd 
#' @importFrom shinyjs show hide useShinyjs
#' @importFrom openCyto gate_flowclust_1d
#' @importFrom flowCore fsApply

mod_gate_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ## SETTING UP THE GFP-BINS
    # hide/show "Add_input" button
    observe({
      if (is.null(r$lower_limit_gfp)) {shinyjs::hide(id = "add_input")}
      else {shinyjs::show(id = "add_input")}
    })
    
    # render uiOutputs sequentially as the user clicks the "add_input" button
    observe({
      if (is.null(input$add_input)) return(NULL)
      if (input$add_input == 1) {
        render_bin_UI(1, c(signif(r$lower_limit_gfp, digits = 3), 100), ns, "First", output, r)
        # output$gfp_bin_1 <- renderUI(numericRangeInput(ns("gfp_range_1"), label = "First GFP bin (LOW)", value = c(signif(r$lower_limit_gfp, digits = 3),100)))
      }
      if (input$add_input == 2) {
        render_bin_UI(2, c(101,350), ns, "Second", output, r)
      #   output$gfp_bin_2 <- renderUI(numericRangeInput(ns("gfp_range_2"), label = "Second GFP bin (MEDIUM)", value = c(101,350)))
      }
      if (input$add_input == 3) {
        render_bin_UI(3, c(351,1000), ns, "Second", output, r)
      #   output$gfp_bin_3 <- renderUI(numericRangeInput(ns("gfp_range_3"), label = "Third GFP bin (HIGH)", value = c(351,1000)))
        shinyjs::hide(id = "add_input")
      }
    }) |> bindEvent(input$add_input)
    

    
    
    ## SET UP A GATE ACCORDING TO THE USER-DEFINED BIN RANGE
    observe({
      if (is.null(input$add_input)) return(NULL)
      
      if (!is.null(input$gfp_range_1) && !is.null(input$gfp_range_2) && !is.null(input$gfp_range_3)) {
        # set gate limits (using user-defined ranges)
        gate_limits <- list(low = list(c(input$gfp_range_1[1], input$gfp_range_1[2])),
                            medium = list(c(input$gfp_range_2[1], input$gfp_range_2[2])),
                            high = list(c(input$gfp_range_3[1], input$gfp_range_3[2])))
        ### for testing
        print(gate_limits)
        print(r$kras_channel())
        
        # generate gates from the bin sizes
        gates <- lapply(gate_limits, function(x) {
          names(x) <- r$kras_channel()
          rectangleGate(x)
        })
        
        # vector of filter names
        filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
        print(gates)
        
        # assign new filterIds to the generated gates (maybe wrap all this into a smart function to reduce codebase)
        for (i in seq_along(gates)) {
          gates[[i]]@filterId <- filter_names[i]
        }
        
        # add gates to the gatingSet
        for (i in seq_along(gates)) {
          gs_pop_add(r$gs, gates[[i]], parent = "MYO+")
        }
        
        recompute(r$gs)
        ### for testing
        plot(r$gs)
        
        data_gfp_low <- gs_pop_get_data(r$gs, y = filter_names[1]) |> cytoset_to_flowSet()
        data_gfp_medium <- gs_pop_get_data(r$gs, y = filter_names[2]) |> cytoset_to_flowSet()
        data_gfp_high <- gs_pop_get_data(r$gs, y = filter_names[3]) |> cytoset_to_flowSet()
        print(data_gfp_low)

      gfp_low_myo_high <- fsApply(data_gfp_low, test_function)
      gfp_low_myo_high <- gfp_low_myo_high[-which(sapply(gfp_low_myo_high, is.null))]

      output$test_plot <- renderPlot({
        ggcyto(r$gs[[5]], aes(x = "RED.R.HLin"), subset = "GFP-low") +
          geom_density(fill = "forestgreen") +
          scale_x_flowjo_biexp() +
          theme_bw() +
          geom_gate(gfp_low_myo_high)
      })

    ## EXTRACT GATED DATA (because we want to apply a mindensity function only on the data in the gate (in this case bin))
    
    output[["test"]] <- renderText(glue("Test works"))
      }
      })
  })}
    
render_bin_UI <- function(bin_number, value, ns, label, output , r) {
  output[[paste0("gfp_bin_", bin_number)]] <- renderUI(numericRangeInput(ns(paste0("gfp_range_", bin_number)), label = paste0(label, "GFP bin"), value = value))
}


test_function <- function(fr) {
  return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density"),
                  error = function(e) NULL))
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

# # illustration how conditionalPanel works
# conditionalPanel(
#   # still a JS expression
#   condition = "input.add_input == 1", 
#   #make sure that input.<input> reacts to (and only to) to input from this module
#   ns = ns, 
#   #what should happen if condition is met
#   checkboxInput(ns("headsonly"), "This text should...."))

## To be copied in the UI
# mod_gate_ui("gate_1")
    
## To be copied in the server
# mod_gate_server("gate_1")
