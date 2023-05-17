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
           # Defining a sidebarLayout in the Gate tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(id = ns("tabset"),
                           tabPanel("GFP bins",
                                    br(),
                                    uiOutput(ns("gfp_bin_1")),
                                    uiOutput(ns("gfp_bin_2")),
                                    uiOutput(ns("gfp_bin_3")),
                                    actionButton(inputId = ns("add_bins"), label = "Add GFP bins", icon("plus")),
                                    actionButton(inputId = ns("confirm_bins"), label = "Confirm", class = "btn-primary"),
                                    actionButton(ns("reset_bins"), "Reset bins", class = "btn-warning")),
                           tabPanel("Split peaks",
                                    br(),
                                    actionButton(ns("split"), "Split now", class = "btn-primary"),
                                    actionButton(inputId = ns("reset_gates"), label = "Reset gates", class = "btn-warning"),
                                    hr(),
                                    br(),
                                    selectInput(ns("controller"),
                                                label = tags$span("Select GFP bin", actionButton(ns("help"), "", icon = icon("info"))),
                                                choices = c("GFP-low", "GFP-medium", "GFP-high")),
                                    ))),

             mainPanel(
               tabsetPanel(id = ns("tabset-mainPanel"),
                 tabPanel("Information", icon = icon("info"),
               h1(strong("How gating works.")),
               div(
                 p("Gating is the most crucial and complex step in this workflow. It consists of", strong("2 essential steps"), ":"),
                 p("1)	Defining the ", strong("number"), " of bins and the", strong("bin size"), ": these bins will become gates.", style = "text-indent: 25px"),
                 p("2)	Inside these bins, calculate the myosin intensity distribution and find a cutoff separating two distinct myosin populations (peaks in a plot)", style = "text-indent: 25px"), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
               ),
               br(),
               div(
                 p("On the left, click on 'Add GFP bin' to add a bin, then enter the bin size. You have to add at least 1 and you are limited to a maximum of 3 bins. Once you have added the desired bins, click on ", span("Confirm", style = "color:#008cba; font-weight:bold"), ". At this point, if you want to change the bin configuration, click on ", span("Reset bins", style = "color:#e99003; font-weight:bold"), ", change the bin configuration, then click ", span("Confirm", style = "color:#008cba; font-weight:bold"), " again."),
                 br(),
                 p(strong("Note:"), "The lower limit of the range of the first GFP bin is defaults to GFP intensity threshold calculated before during curation. You can lower this value, but your data has essentially already been gated to exclude such low intensities because they're unspecific and probably originate from autofluorescence."),
                 p("To proceed, click on the ", strong("Split peaks"), " tab at the top of the left side panel."),
                 style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
                 ),
               br(),
               hr(),
               br(),
               div(

                 p("When we look at the myosin distribution inside the bins just created, a pattern emerges. In fact, two populations can be distinguished:"),
                 p("-	Low myosin expression: for C2C12, these cells can be seen as", strong("progenitors"), style = "text-indent: 25px"),
                 p("-	High myosin expression: for C2C12, these cells can be seen as", strong("differentiated myocytes"), style = "text-indent: 25px"),

                 p("Inside the app, we can find the threshold separating these two populations by clicking on the", span("Split", style = "color:#008cba; font-weight:bold"), " button."),
                 p(strong("Note:"), " at this point, if you want to change the bin configuration, you have to click on", span("Reset gates", style = "color:#e99003; font-weight:bold"), ", switch to the GFP bins tab and start over."),
                 style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
                 )),
               
               tabPanel("Plot",
               textOutput(ns("test")),
               DTOutput(ns("individual_FCS")),
               plotOutput(ns("myosin_splittedPeaks"))))
)))}

#' gate Server Functions
#' @noRd
#' @importFrom shinyjs show hide useShinyjs
#' @importFrom openCyto gate_flowclust_1d
#' @importFrom flowCore fsApply
#' @importFrom flowWorkspace gs_pop_remove
#'
mod_gate_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


# Modals, help buttons, etc. ----------------------------------------------
    ## Plot panel: Help button
    observe({
      showModal(modalDialog(p("Select the GFP bin for which you want to display the myosin intensities.")))
    }) |> bindEvent(input$help)

    ## "Add_bins" button
    observe({
      if (is.null(r$lower_limit_gfp)) {shinyjs::hide(id = "add_bins")}
      else {shinyjs::show(id = "add_bins")}
    })

    # "Confirm" and "Add GFP bins" buttons
    observe({
      shinyjs::hide(id = "confirm_bins")
      shinyjs::hide(id = "add_bins")
    }) |> bindEvent(input$confirm_bins)

    modal_confirm_bins <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting your GFP bins",
      footer = tagList(
        actionButton(ns("cancel_bin_reset"), "Cancel"),
        actionButton(ns("confirm_bin_reset"), "Delete", class = "btn btn-danger")))

    observe({
      showModal(modal_confirm_bins)
    }) |> bindEvent(input$reset_bins) #show modal when user clicks reset_bins button

    observe({
      removeModal()
    }) |> bindEvent(input$cancel_bin_reset) #remove modal when user decides to CANCEL bin reset

    observe({
      shinyjs::show(id = "confirm_bins")
      shinyjs::show(id = "add_bins")
      showNotification("Bins were successfully reset.")
      removeModal()
    }) |> bindEvent(input$confirm_bin_reset)

    # "Gate" button
    modal_confirm_gates <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting your gates",
      footer = tagList(
        actionButton(ns("cancel_gate_reset"), "Cancel"),
        actionButton(ns("confirm_gate_reset"), "Delete", class = "btn btn-danger")))

    observe({
      shinyjs::hide(id = "split")
      shinyjs::show(id = "reset_gates")
    }) |> bindEvent(input$split)

    observe({
      shinyjs::hide(id = "reset_gates")
      shinyjs::show(id = "split")
      showNotification("Gates were successfully reset.")
      removeModal()
    }) |> bindEvent(input$confirm_gate_reset)

    observe({
      removeModal()
    }) |> bindEvent(input$cancel_gate_reset)

    observe({
      showModal(modal_confirm_gates)
    }) |> bindEvent(input$reset_gates)

# Reset bins/gates if user clicks on the Reset button(s)
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
    }) |> bindEvent(input$confirm_bin_reset)

    ### this has to be modified!!!! see issue #13
    observe({
      if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-low"))) {gs_pop_remove(r$gs, "GFP-low")}
      if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-medium"))) {gs_pop_remove(r$gs, "GFP-medium")}
      if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-high"))) {gs_pop_remove(r$gs, "GFP-high")}
      for (i in seq_along(gates())) {
        gs_pop_add(r$gs, gates()[[i]], parent = "MyHC+")
      }
      recompute(r$gs)
      #plot(r$gs)
    }) |> bindEvent(input$confirm_gate_reset)

# Computations ------------------------------------------------------------
    # capture the number of times the button is clicked inside a reactive expression: add_bins_clicks()
    # run the render_bin_UI function according to the value of the reactive expression

    add_bins_clicks <- reactive({input$add_bins})

    observe({
      switch(add_bins_clicks(),
             render_bin_UI(1, c(signif(r$lower_limit_gfp, digits = 3), 100), ns, "First", output, r),
             render_bin_UI(2, c(101,350), ns, "Second", output, r),
             render_bin_UI(3, c(351,1000), ns, "Third", output, r))
      if (add_bins_clicks() == 3) shinyjs::hide(id = "add_bins")
    })

    # capture gate ranges (user input) inside a reactive expression: gate_limits()
    gate_limits <- reactive({
      withProgress(message = "Calculating gate limits...", {
      x <- list(low = if(!is.null(input$gfp_range_1)) list(input$gfp_range_1),
                medium = if(!is.null(input$gfp_range_2)) list(input$gfp_range_2) else {list(NA)},
                high = if(!is.null(input$gfp_range_3)) list(input$gfp_range_3) else {list(NA)})
      x[!sapply(x, is.na)]})
    })

    # for debugging
    message("printing gate_limits")
    observe({
      if (!is.null(gate_limits())) print(gate_limits())
    }) |> bindEvent(input$confirm_bins)

    ### this is not working properly: error in !: invalid argument type
    # message("printing gate_limits")
    # observe({
    #   if(!is.null(gate_limits())) print(gate_limits())
    # })

    # use the gate_limits() reactive expression to create a gate
    gates <- reactive({
      withProgress(message = "Creating gates...", {
      if (is.null(gate_limits())) return(NULL)
      filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
      y <- lapply(gate_limits(), function(x) {
        names(x) <- r$ch_kras()
        rectangleGate(x)})
      for (i in seq_along(y))
        y[[i]]@filterId <- filter_names[[i]]
      return(y)})
    })

    #observe({print(gates())}) # i guess this is not working for the same reasons the printing above does not work

    # add gates to the gatingSet
    observe({
      for (i in seq_along(gates())) {
        gs_pop_add(r$gs, gates()[[i]], parent = "MyHC+")
      }

      recompute(r$gs)
      #plot(r$gs)
    }) |> bindEvent(input$confirm_bins)  # input$confirm_bins used here, after this the button should disappear (done in line 73)

    # extract gates data and split myosin peak
    ## nice: if any of reactive expressions below are called and the input doesn't exist (e.g gfp_range_2 when only first GFP bin added),
    ## expression evaluates to NULL
    ## note: below we don't add anything to the gatingSet. the reactive expressions return a gate "rectangular gate with dimensions....". We later
    ## add this gate to the gatingSet

#' @importFrom stringr str_detect
#' @importFrom flowWorkspace gs_get_pop_paths

gfp_low_myo_high <- reactive({
  req(r$gs)
  withProgress(message = "Retrieving data from GFP-low bin...", {
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-low"))) {       # this is to make sure that GFP-low gate exists!
  getData_splitPeak(r = r, gs = r$gs, bin = "GFP-low", filter_name = "GFP-low-MYO-high")
  }})
})

gfp_medium_myo_high <- reactive({
  req(r$gs)
  withProgress(message = "Retrieving data from GFP-low bin...", {
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-medium"))) {        # this is to make sure that GFP-medium gate exists!
    getData_splitPeak(r = r, gs = r$gs, bin = "GFP-medium", filter_name = "GFP-medium-MYO-high")
  }})
})

gfp_high_myo_high <- reactive({
  req(r$gs)
  withProgress(message = "Retrieving data from GFP-high bin...", {
  if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-high"))) {        # this is to make sure that GFP-high gate exists!
    getData_splitPeak(r = r, gs = r$gs, bin = "GFP-high", filter_name = "GFP-high-MYO-high")
  }})
})


# reactive expression to use inside geom_gate: gate_myosin_plot()
## we want the geom_gate to contain the gate that the user chooses in the controller (a selectInput defined in the UI)
## ideally the controller only displays the possible options, e.g. if user adds "GFP-low" bin he can't select GFP-medium on the controller

gate_myosin_plot <- reactive({
  ## first check that the right conditions are met to compute this reactive
  if (input$controller == "GFP-low") {req(gfp_low_myo_high())}
  if (input$controller == "GFP-medium") {req(gfp_medium_myo_high())}
  if (input$controller == "GFP-high") {req(gfp_high_myo_high())}
  ## depending on the state of the controller, a different gate should be assigned to this reactive expression
  switch(input$controller,
         "GFP-low" = gfp_low_myo_high(),
         "GFP-medium" = gfp_medium_myo_high(),
         "GFP-high" = gfp_high_myo_high())
})

# reactive expression capturing the state of the controlls: subset()
subset <- reactive(input$controller)

# # for debugging
# observe({
#   message("second printing")
#   # print(gate_myosin_plot())
# }) |> bindEvent(input$split)

# add the gates computed by getData_splitPeak() to the gatingSet: custom function add_gate()
## recompute(r$gs) already wrapped inside add_gate()
## plot(r$gs) doesn't work properly now: I think it only plots the gates that are common across all samples?! However, we will add gates to individual samples using add_gate() as the sample names in our gatingSet do not necessarily match those in the gates we add. Remember that we removed gates that evaluate to NULL. Same problem with gs_get_pop_paths(): workaround using the reactive expression final_output().
## this addition to the gatingSet is under the control of input$split: documents this properly in the mainPanel and README

observe({
  if (!is.null(gfp_low_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_low_myo_high(), parent = "GFP-low")}
  if (!is.null(gfp_medium_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_medium_myo_high(), parent = "GFP-medium")}
  if (!is.null(gfp_high_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_high_myo_high(), parent = "GFP-high")}
}) |> bindEvent(input$split)


## output plot: a  plot showing splitted peaks for the gate chosen by the user using the controller.
output$myosin_splittedPeaks <- renderPlot({
  req(r$gs, selected_rows(), gate_myosin_plot(), subset())
  withProgress(message = "Plotting your data...", {
  plot_myosin_splittedPeaks(r = r, gs = r$gs[[selected_rows()]], density_fill = "pink", gate = gate_myosin_plot(), subset = subset())})
}, res = 120)

output[["test"]] <- renderText(glue("Test works"))

observe({
  modal_confirm_bins <- modalDialog(
    p("Your bins have been successfully added! Click on ", span("Split now", style = "color:#008cba; font-weight:bold"), " to split your MyHC peaks."),
    title = "Done!"
  )
  showModal(modal_confirm_bins)
  updateTabsetPanel(inputId = "tabset", selected = "Split peaks")
}) |> bindEvent(input$confirm_bins)

flowSet_pData <- reactive({
  req(r$fs)
  pData(r$fs)
  })

selected_rows <- reactive(input$individual_FCS_rows_selected)

output$individual_FCS <- renderDT({flowSet_pData()}, selection = list(target = "row", selected = 1, mode = "multiple"),
                                  rownames = FALSE,
                                  class = "cell-border stripe")

observe({
  updateTabsetPanel(inputId = "tabset-mainPanel", selected = "Plot")
}) |> bindEvent(input$split)
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
  ## capture the indices of the the elements that are NULL, assign indices to test variable
  test <- which(sapply(data, is.null))
  ## when no NULLs (test variable of size 0), return the input data
  if (sum(test) == 0) {
    return(data)
  }
  ## when NULLs identified (test variable of size != 0), return the input data WITHOUT the positions that were NULL
  else {
    return(data[-test])
  }
}

plot_myosin_splittedPeaks <- function(r, gs, subset, density_fill, gate) {
    ggcyto(gs, aes(x = "RED.R.HLin"), subset) +
      geom_histogram(bins = 50, fill = density_fill, color = "black") +
      scale_x_flowjo_biexp() +
      theme_bw() +
      geom_gate(gate)
}

getData_splitPeak <- function(r, gs, bin, filter_name) {
  ## fetch the data we need: bin = name of the gate from which we want the data. We use it to access "GFP-low", "GFP-medium" and "GFP-high" gates
  x <- gs_pop_get_data(gs, bin) |> cytoset_to_flowSet()
  ## x is a flowSet. To each flowFrame, we apply the function tryCatch(). If no error, returns a gate that cuts between myosin peaks. If error, the respective gate evaluates to NULL
  x <- fsApply(x, function(fr, filterId = filter_name) {
    return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density", filterId = filterId), error = function(e) NULL))
  })
  ## we don't want the gates that evaluate to NULL, so we remove them. remove_null_from_list() is a custom function.
  return(remove_null_from_list(x))
}

# custom function was needed to add these gates. Some gates are REMOVED using the remove_null_from_list() function and then the function gs_pop_add from openCyto does not work anymore! It requires that the names of the datasets in gs and the name of the gates match! e.g. if gs has dataset1 but there is no gate for dataset1 (because it was NULL and therefore removed using remove_null_from_list) the function does not work. this is a workaround

add_gate <- function(r, gs, gate, parent) {
  gate_names <- names(gate)
  gatingSet_names <- sampleNames(gs)
  for (i in seq_along(gatingSet_names)) {
    if (gatingSet_names[i] %in% gate_names) {
      x <- which(gatingSet_names[i] == gate_names) # gives the index of the name in gate_names that matches gatingSet_names
      gs_pop_add(r$gs[[i]], gate = gate[[x]], parent = parent)
    }
  }
  recompute(r$gs)
}


#' #' @importFrom stringr str_to_upper
#' create_bin_button <- function (channel, bin_number, label, ns, lower_limit) {
#'   if (channel == "gfp") {
#'     if (bin_number == 1) {
#'     conditionalPanel(condition = paste0("input.add_bins >= ", bin_number),
#'                      ns = ns,
#'                      numericRangeInput(ns(paste0(channel, "_range_", bin_number)),
#'                                        label = paste0(label," ", str_to_upper(channel), " ", "bin"),
#'                                        value = c(signif(lower_limit, digits = 3),100)))
#'     }
#'     else if (bin_number == 2) {
#'       conditionalPanel(condition = paste0("input.add_bins >= ", bin_number),
#'                        ns = ns,
#'                        numericRangeInput(ns(paste0(channel, "_range_", bin_number)),
#'                                          label = paste0(label," ", str_to_upper(channel), " ", "bin"),
#'                                          value = c(101,350)))
#'     }
#'     else if (bin_number == 3) {
#'       conditionalPanel(condition = paste0("input.add_bins >= ", bin_number),
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
#   condition = "input.add_bins == 1",
#   #make sure that input.<input> reacts to (and only to) to input from this module
#   ns = ns,
#   #what should happen if condition is met
#   checkboxInput(ns("headsonly"), "This text should...."))

## To be copied in the UI
# mod_gate_ui("gate_1")

## To be copied in the server
# mod_gate_server("gate_1")
