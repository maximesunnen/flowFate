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
                actionButton(inputId = ns("add_input"), label = "Add GFP bins", icon("plus"), class = "btn-primary"),
                actionButton(inputId = ns("gate"), label = "Gate now", class = "btn-primary"),
                actionButton(ns("Delete"), "Restart binning", class = "btn-danger"),
                
                selectInput(ns("controller"), "Show", choices = c("GFP-low", "GFP-medium", "GFP-high"))
             ),
             
             mainPanel(
               # header and text description of gating ---------------------------------
               h1("How gating works."),
               p(style = "text-align:justify;color
                 :black;background-color:papayawhip;padding:15px;border-radius:10px"),
               # outputs
               textOutput(ns("test")),
               
               tabsetPanel(
                 id = ns("switcher"),
                 type = "hidden",
                 tabPanelBody("GFP-low", plotOutput(ns("gfp_low_myo_plot"))),
                 tabPanelBody("GFP-medium", plotOutput(ns("gfp_medium_myo_plot"))),
                 tabPanelBody("GFP-high", plotOutput(ns("gfp_high_myo_plot")))
               )
             )))}
    
#' gate Server Functions
#' @noRd 
#' @importFrom shinyjs show hide useShinyjs
#' @importFrom openCyto gate_flowclust_1d
#' @importFrom flowCore fsApply
#' @importFrom flowWorkspace gs_pop_remove

mod_gate_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    observe({
      updateTabsetPanel(inputId = "switcher", selected = input$controller)
    }) |> bindEvent(input$controller)
  
    ## SETTING UP THE GFP-BINS
    # hide/show "Add_input" button
    observe({
      if (is.null(r$lower_limit_gfp)) {shinyjs::hide(id = "add_input")}
      else {shinyjs::show(id = "add_input")}
    })
    
    observe({
      shinyjs::hide(id = "gate")
    }) |> bindEvent(input$gate)
    
    observe({
      shinyjs::show(id = "gate")
    }) |> bindEvent(input$ok)
    
    modal_confirm <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting gates",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Restart", class = "btn btn-danger")
      )
    )
    
    observe({
      showModal(modal_confirm)
    }) |> bindEvent(input$Delete)
    
    observe({
      showNotification("Bins were successfully reset.")
      removeModal()
    }) |> bindEvent(input$ok)
    
    observe({
      removeModal()
    }) |> bindEvent(input$cancel)
    
    observe({
      if (is.null(input$gfp_range_2)) {
        gs_pop_remove(r$gs, "GFP-low")
      }
      else if (is.null(input$gfp_range_3)) {
        gs_pop_remove(r$gs, "GFP-low")
        gs_pop_remove(r$gs, "GFP-medium")
      }
      else {
        gs_pop_remove(r$gs, "GFP-low")
        gs_pop_remove(r$gs, "GFP-medium")
        gs_pop_remove(r$gs, "GFP-high")
      }
    }) |> bindEvent(input$ok)
    
    
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
      if (is.null(input$gfp_range_1)) return(NULL)
      else if (is.null(input$gfp_range_2)) {
        # set up gate limits (using user-defined ranges)
        gate_limits <- list(low = list(input$gfp_range_1))
      }
      else if (is.null(input$gfp_range_3)) {
        gate_limits <- list(low = list(input$gfp_range_1),
                            medium = list(input$gfp_range_2))
      }
      
      else {gate_limits <- list(low = list(input$gfp_range_1),
                                medium = list(input$gfp_range_2),
                                high = list(input$gfp_range_3))
      }
      message("here are the values of gate_limits")
      print(gate_limits)
      # generate gates from bins
      gates <- lapply(gate_limits, function(x) {
        names(x) <- r$ch_kras()
        rectangleGate(x)
      })
        
        # assign filterIds to gates
        filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
        for (i in seq_along(gates)) {
          gates[[i]]@filterId <- filter_names[i]
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

      if (is.null(input$gfp_range_2)) {
        gfp_low_myo_high <- getData_splitPeak(r$gs, "GFP-low")
        message("only computed gfp_low_myo_high")
      }
      else if (is.null(input$gfp_range_3)) {
        gfp_low_myo_high <- getData_splitPeak(r$gs, "GFP-low")
        gfp_medium_myo_high <- getData_splitPeak(r$gs, "GFP-medium") 
        ## problem is here: outputs a "named list()": this only happens when all the datasets are NULL: the function remove_null_from_list then outputs a named list()
        message("computed gfp_low_myo_high as well as gfp_medium_myo_high")

      }
      else {
        gfp_low_myo_high <- getData_splitPeak(r$gs, "GFP-low")
        gfp_medium_myo_high <- getData_splitPeak(r$gs, "GFP-medium")
        gfp_high_myo_high <- getData_splitPeak(r$gs, "GFP-high")  ## outputs also a "named list()": this only happens when all the datasets are NULL: the function remove_null_from_list then outputs a named list()
        message("computed gfp_low_myo_high as well as gfp_high_myo_high")
        print(gfp_low_myo_high)
        print(gfp_medium_myo_high)
        print(gfp_high_myo_high)
      }
      
      #need to change x_axis = "RED.R.HLin" to something like r$ch_myhc()
      #could change also that subset = input$controller
      if (is.null(input$gfp_range_2)) {
        output$gfp_low_myo_plot <- plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_low_myo_high, subset = "GFP-low")
      }
      else if (is.null(input$gfp_range_3)) {
        output$gfp_low_myo_plot <- plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_low_myo_high, subset = "GFP-low")
        output$gfp_medium_myo_plot <-  plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_medium_myo_high, subset = "GFP-medium")
        }
        else {
          output$gfp_low_myo_plot <- plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_low_myo_high, subset = "GFP-low")
          output$gfp_medium_myo_plot <- plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_medium_myo_high, subset = "GFP-medium")
          output$gfp_high_myo_plot <- plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gfp_high_myo_high, subset = "GFP-high")
        }
    }) |> bindEvent(input$gate)
    

      
    ## EXTRACT GATED DATA (because we want to apply a mindensity function only on the data in the gate (in this case bin))
    
    output[["test"]] <- renderText(glue("Test works"))
  })}


render_bin_UI <- function(bin_number, value, ns, label, output , r) {
  output[[paste0("gfp_bin_", bin_number)]] <- renderUI(numericRangeInput(ns(paste0("gfp_range_", bin_number)), label = paste0(label, " GFP bin"), value = value))
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

plot_myosin_splittedPeaks <- function(r, gs, subset, density_fill, gate) {
  renderPlot({
    ggcyto(gs, aes(x = "RED.R.HLin"), subset = subset) +
      geom_density(fill = density_fill) +
      scale_x_flowjo_biexp() +
      theme_bw() +
      geom_gate(gate)
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
