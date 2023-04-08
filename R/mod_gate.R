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
                actionButton(inputId = ns("gate"), label = "Gate now")
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
      }
      if (input$add_input == 2) {
        render_bin_UI(2, c(101,350), ns, "Second", output, r)
      }
      if (input$add_input == 3) {
        render_bin_UI(3, c(351,1000), ns, "Third", output, r)
        shinyjs::hide(id = "add_input")
      }
    }) |> bindEvent(input$add_input)

    ## SET UP A GATE ACCORDING TO THE USER-DEFINED BIN RANGE
    observe({
      if (is.null(input$gfp_range_3)) return(NULL)
      else {
        # set up gate limits (using user-defined ranges)
        gate_limits <- list(low = list(input$gfp_range_1), medium = list(input$gfp_range_2), 
                            high = list(input$gfp_range_3))
        # generate gates from bins
        gates <- lapply(gate_limits, function(x) {
          names(x) <- r$kras_channel()
          rectangleGate(x)
        })
        
        # assign filterIds to gates
        filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
        for (i in seq_along(gates)) {
          gates[[i]]@filterId <- filter_names[i]
        }
      }
      print(gates)
      
      # ADD GATES TO GATINGSET
      for (i in seq_along(gates)) {
        gs_pop_add(r$gs, gates[[i]], parent = "MYO+")
      }
      recompute(r$gs)
      View(gs_pop_get_count_fast(r$gs))

      # EXTRACT GATED DATA FOR PEAK SPLITTING
      getData_splitPeak <- function(gs, bin) {
        x <- gs_pop_get_data(gs, bin) |> cytoset_to_flowSet()
        x <- fsApply(x, test_function)
        return(remove_null_from_list(x))
      }
    
      gfp_low_myo_high <- getData_splitPeak(r$gs, "GFP-low")
      gfp_medium_myo_high <- getData_splitPeak(r$gs, "GFP-medium")
      gfp_high_myo_high <- getData_splitPeak(r$gs, "GFP-high")

      output$test_plot <- renderPlot({
        ggcyto(r$gs[[5]], aes(x = "RED.R.HLin"), subset = "GFP-low") +
          geom_density(fill = "pink") +
          scale_x_flowjo_biexp() +
          theme_bw() +
          geom_gate(gfp_low_myo_high)
      })
    }) |> bindEvent(input$gate)
    

      
    ## EXTRACT GATED DATA (because we want to apply a mindensity function only on the data in the gate (in this case bin))
    
    output[["test"]] <- renderText(glue("Test works"))

    
  })}


render_bin_UI <- function(bin_number, value, ns, label, output , r) {
  output[[paste0("gfp_bin_", bin_number)]] <- renderUI(numericRangeInput(ns(paste0("gfp_range_", bin_number)), label = paste0(label, "GFP bin"), value = value))
}


test_function <- function(fr) {
  return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density"),
                  error = function(e) NULL))
}

remove_null_from_list <- function(data) {
  test <- which(sapply(data, is.null))
  if (sum(test) == 0) {
    return(data)
  }
  else {
    return(data[-test])
  }
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
