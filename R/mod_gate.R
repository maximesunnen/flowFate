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
           br(),
           br(),
           actionButton(inputId = ns("gate"), label = "Gate now", class = "btn-primary"),
           actionButton(ns("Delete"), "Restart binning", class = "btn-danger"),
           br(),
           br(),
           br(),
           selectInput(ns("controller"), "GFP bin", choices = c("GFP-low", "GFP-medium", "GFP-high")),
           actionButton(ns("plot"), "plot"),
           actionButton(ns("split"), "Split now")),
  
           mainPanel(
             # header and text description of gating ---------------------------------
             h1("How gating works."),
             p(style = "text-align:justify;color
                 :black;background-color:papayawhip;padding:15px;border-radius:10px"),
             # outputs
             textOutput(ns("test")),
             
             plotOutput(ns("myosin_splittedPeaks")),
           )
           )
           )
  }
    
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


# SET UP A GATE ACCORDING TO THE USER-DEFINED BIN RANGE -------------------

    gate_limits <- reactive({
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
    })
    
    message("printing gate_limits")
    observe({print(gate_limits())})

# GENERATE GATES FROM BINS ------------------------------------------------

  gates <- reactive({
    if (is.null(gate_limits())) return(NULL)
    filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
    y <- lapply(gate_limits(), function(x) {
    names(x) <- r$ch_kras()
    rectangleGate(x)})
    for (i in seq_along(y))
    y[[i]]@filterId <- filter_names[[i]]
    return(y)
    })

  observe(print(gates()))


# ADD GATES TO GATINGSET --------------------------------------------------

  observe({
    for (i in seq_along(gates())) {
      gs_pop_add(r$gs, gates()[[i]], parent = "MYO+")
    }
    recompute(r$gs)
    plot(r$gs)
  }) |> bindEvent(input$gate)
  

# EXTRACT GATED DATA FOR PEAK SPLITTING -----------------------------------
  
#' @importFrom stringr str_detect
#' @importFrom flowWorkspace gs_get_pop_paths

  ## very nice: if we call any of reactive expressions below and the respective input doesn't exist (e.g added only first GFP bin), expression evaluates to NULL

gfp_low_myo_high <- reactive({
  req(r$gs)
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-low"))) {
  getData_splitPeak(r = r, gs = r$gs, bin = "GFP-low")
  }
})

gfp_medium_myo_high <- reactive({
  req(r$gs)
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-medium"))) {
    getData_splitPeak(r = r, gs = r$gs, bin = "GFP-medium")
  }
})

gfp_high_myo_high <- reactive({
  req(r$gs)
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-high"))) {
    getData_splitPeak(r = r, gs = r$gs, bin = "GFP-high")
  }
})

# for debugging
observe({
  message("first printing")
  print(gfp_medium_myo_high())
}) |> bindEvent(input$plot)

# SET UP REACTIVE EXPRESSION FOR THE GATE TO BE PLACED INSIDE GEOM --------
## ideally we make it that the controller only has the options that are possible, e.g. if user adds 1 gfp bin he can't select GFP-medium on the controller
## 
gate_myosin_plot <- reactive({
  if (input$controller == "GFP-low") {req(gfp_low_myo_high())}
  if (input$controller == "GFP-medium") {req(gfp_medium_myo_high())}
  if (input$controller == "GFP-high") {req(gfp_high_myo_high())}
  switch(input$controller,
         "GFP-low" = gfp_low_myo_high(),
         "GFP-medium" = gfp_medium_myo_high(),
         "GFP-high" = gfp_high_myo_high())
})
subset <- reactive(input$controller)

    # for debugging
    observe({
      message("second printing")
      print(gate_myosin_plot())
      print(subset())
    }) |> bindEvent(input$plot)

    output$myosin_splittedPeaks <- renderPlot({
      plot_myosin_splittedPeaks(r = r, gs = r$gs, density_fill = "pink", gate = gate_myosin_plot(), subset = subset())
      }) |> bindEvent(input$plot)

    ## NOTE: we have not added the gate that splits the two peaks to the gatingSet!!! we only visually display it to the user
    observe({
      if(!is.null(gfp_low_myo_high())) {gs_pop_add(r$gs, gfp_low_myo_high(), parent = "GFP-low")}
      if(!is.null(gfp_medium_myo_high())) {gs_pop_add(r$gs, gfp_medium_myo_high(), parent = "GFP-medium")}
      if(!is.null(gfp_high_myo_high())) {gs_pop_add(r$gs, gfp_high_myo_high(), parent = "GFP-high")}
      recompute(r$gs)
      plot(r$gs)
    }) |> bindEvent(input$split)
    
    output[["test"]] <- renderText(glue("Test works"))
  })}


# CUSTOM FUNCTIONS --------------------------------------------------------
render_bin_UI <- function(bin_number, value, ns, label, output , r) {
  output[[paste0("gfp_bin_", bin_number)]] <- renderUI(numericRangeInput(ns(paste0("gfp_range_", bin_number)), label = paste0(label, " GFP bin"), value = value))
}

# here we might need to provide a filterId, not sure if it works if we provide it in the "higher-order" function in which this function is called because of environments
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
    ggcyto(gs, aes(x = "RED.R.HLin"), subset = subset) +
      geom_density(fill = density_fill) +
      scale_x_flowjo_biexp() +
      theme_bw() +
      geom_gate(gate)
}

getData_splitPeak <- function(r, gs, bin) {
  x <- gs_pop_get_data(gs, bin) |> cytoset_to_flowSet()
  x <- fsApply(x, test_function)
  return(remove_null_from_list(x))
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
