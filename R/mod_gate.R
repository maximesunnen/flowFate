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
             # Defining the sidebarPanel
             sidebarPanel(
               tabsetPanel(id = ns("tabset"),
                           tabPanel("GFP bins",
                                    br(),
                                    # Defining the numeric range inputs of the gfp bins. This needs to be an uiOutput since I use r$lower_limit_gfp as the lower limit of the first bin
                                    uiOutput(ns("gfp_bin_1")),
                                    uiOutput(ns("gfp_bin_2")),
                                    uiOutput(ns("gfp_bin_3")),
                                    uiOutput(ns("test")),
                                    # Defining the action buttons that allow the user to add a bin, confirm the bins or reset the bins
                                    actionButton(inputId = ns("add_bins"), label = "Add GFP bin", icon("plus")),
                                    actionButton(inputId = ns("confirm_bins"), label = "Confirm", class = "btn-primary"),
                                    actionButton(inputId = ns("reset_bins"), label = "Reset bins", class = "btn-warning")),
                           tabPanel("Split peaks",
                                    br(),
                                    # Defining the action button that allow the user to split their MyHC peaks or reset the splitting
                                    actionButton(inputId = ns("split"), label = "Split", class = "btn-primary"),
                                    # actionButton(inputId = ns("reset_gates"), label = "Reset gates", class = "btn-warning"), hr(), br(),
                                    # Defining the input selection for the GFP bin to display in the output plot. Info button to the right of the label to give the user some help
                                    selectInput(ns("controller"),
                                                label = tags$span("Select GFP bin", actionButton(ns("help"), "", icon = icon("info"))),
                                                choices = c("GFP-low", "GFP-medium", "GFP-high")),
                                    # Defining the table output where the user can select the individual datasets to display in the output plot
                                    DTOutput(ns("individual_FCS"))
                           ))),

             # Defining the mainPanel
             mainPanel(
               tabsetPanel(id = ns("tabset-mainPanel"),
                 tabPanel("Information", icon = icon("info"),
               h1(strong("How gating works.")),
               div(
                 p("Gating is the most crucial and complex step in this workflow. It consists of", strong("2 essential steps"), ":"),
                 p("1)	Defining bin ", strong("number"), " and bin ", strong("size"), ": these bins will become gates.", style = "text-indent: 25px"),
                 p("2)	For each bin, find a threshold separating two distinct myosin populations (identified as separate peaks)", style = "text-indent: 25px"), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
               ),
               br(),
               div(
                 p("On the left, click ", strong("'Add GFP bin'"), ", then enter the bin size. The maximum number of bins you can add is three and you need to add at least one. Once bins have been added, click ", span("Confirm", style = "color:#008cba; font-weight:bold"), ". A pop-up window appears indicating that binning was successful and the active tab in the sidebar will automatically be updated from 'GFP bins' to 'Split peaks'."),
                 p(strong("Note 1:"), "If you want to change the bin configuration, click ", span("Reset bins", style = "color:#e99003; font-weight:bold"), ", change the bin configuration, then click ", span("Confirm", style = "color:#008cba; font-weight:bold"), " again."),
                 p(strong("Note 2:"), "The lower limit of the 'GFP low' bin defaults to the intensity threshold calculated during curation. Lowering this value does not change anything as cells with such low GFP intensities have essentially already been removed by gating."),
                 style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"),
               br(),
               hr(),
               br(),
               div(

                 p("The presence of two peaks in the MyHC intensity distribution suggests the presence of two distinct populations:"),
                 p("-	Low myosin expression: for C2C12, these are proliferating", strong("progenitors"), style = "text-indent: 25px"),
                 p("-	High myosin expression: for C2C12, these are ", strong("differentiated myocytes"), style = "text-indent: 25px"),

                 p("The exact threshold separating these two peaks can change from sample to sample. Click ", span("Split", style = "color:#008cba; font-weight:bold"), " to apply a data-driven gating function that determines this threshold for each sample individually. You will automatically be redirected from the 'Information' tab to the 'Plot' tab."),
                 p("On the left, under 'Select GFP bin', select the GFP bin you're interested in. Then select the appropriate dataset in the table. The appearing plot displays the MyHC distribution with the peak-splitting threshold in red. Multiple dataset selections are allowed to facilitate comparisons."),
                 p(strong("Note:"), " If you want to change the bin configuration, you need to click ", span("Reset gates", style = "color:#e99003; font-weight:bold"), ", switch to the 'GFP bins' tab and start again."),
                 style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
                 )),

               tabPanel("Plot",
                        br(),
                        plotOutput(ns("myosin_splittedPeaks"))))
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

# Computations ------------------------------------------------------------
    # capture the number of times the button is clicked inside a reactive expression: add_bins_clicks()
    # run the render_bin_UI function according to the value of the reactive expression
    # to not get into a reactive loop, let's use a reactive value

    # add_bins_clicks <- reactive({input$add_bins})
    
    # making a reactiveValues (clicks) with 2 elements, count and x. Update clicks$count every time input$add_bins is clicked to capture the number of times it is clicked.
    clicks <- reactiveValues(count = 0)
    
    observe({
      clicks$count <- clicks$count + 1
    }) |> bindEvent(input$add_bins)
    
    # Depending on the value of clicks$count (i.e how many times input$add_bins is clicked), add GFP bins to the UI.
    ### Remove the input$add_bins button once clicks$count is equal to 3.
    observe({
      switch(clicks$count,
             render_bin_UI(1, c(signif(r$lower_limit_gfp, digits = 3), 100), ns, "low", output, r),
             render_bin_UI(2, c(101,350), ns, "medium", output, r),
             render_bin_UI(3, c(351,1000), ns, "high", output, r))
      if (clicks$count == 3) {
        shinyjs::hide(id = "add_bins")
      }
      else {
        shinyjs::show(id = "add_bins")
      }
    })
    



    # making a reactive expression gate_limits() that captures the user's gate ranges inside a reactive expression
    ## in order for subsequent code to work this has to be a nested list... can't really remember why and unfortunately I did not write this down...
    gate_limits <- reactive({
      withProgress(message = "Calculating gate limits...", {
      x <- list(low = if(!is.null(input$gfp_range_1)) list(input$gfp_range_1),
                medium = if(!is.null(input$gfp_range_2)) list(input$gfp_range_2) else {list(NA)},
                high = if(!is.null(input$gfp_range_3)) list(input$gfp_range_3) else {list(NA)})
      x[!sapply(x, is.na)]})
    })

    # making a reactive expression gates() to create GFP-low(-medium;-high) gates, using user input captured in the reactive expression gate_limits()
    gates <- reactive({
      withProgress(message = "Creating gates...", {
        # stop execution if gate_limits() is NULL
        if (is.null(gate_limits())) return(NULL)
        # if gate_limits() is not NULL, execute code below
        filter_names <- c("GFP-low", "GFP-medium", "GFP-high")
        y <- lapply(gate_limits(), function(x) {
          names(x) <- r$ch_kras()
          rectangleGate(x)})
        for (i in seq_along(y)){
          y[[i]]@filterId <- filter_names[[i]]
        }
        return(y)
      })
    })

    # making a reactive value observer.state.bins to capture whether or not the GFP-low/medium/high bins have been added to the gatingSet
    observer.state.bins <- reactiveVal(FALSE)
    
    # making an observer that adds the GFP-low/medium/high bins to the gatingSet; update the observer.state.bins to TRUE at the end
    ### under the control of input$confirm_bins so it is not executed every time any of the inputs, reactive expressions/values changes
    observe({
      for (i in seq_along(gates())) {
        gs_pop_add(r$gs, gates()[[i]], parent = "GFP+")
      }
      recompute(r$gs)
      observer.state.bins(TRUE)
      showModal(modal_confirm_bins)
      updateTabsetPanel(inputId = "tabset", selected = "Split peaks")
    }) |> bindEvent(input$confirm_bins)
    
#' @importFrom stringr str_detect
#' @importFrom flowWorkspace gs_get_pop_paths

    # making reactive expressions that
    #   (1) detects if a specific GFP bin is present
    #   (2) if GFP bin present, extracts gated data from the entire gatingSet (and only of a specific GFP-bin), applies clustering function which finds a cutoff between the two MyHC              peaks, returns these cutoffs as a list
    ### getData_splitPeak is a complex custom function which also uses tryCatch() and eliminates resulting NULLs [see end of document for details]
    ### If any of the reactive expressions below are called and the input doesn't exist (e.g gfp_range_2 when only first GFP bin added), the expression evaluates to NULL
    ### Note: these reactive expressions don't add anything to the gatingSet but only return a gate ("rectangular gate with dimensions....") that is later added to the gatingSet

    gfp_low_myo_high <- reactive({
      req(r$gs)
      withProgress(message = "Retrieving data from GFP-low bin...", {
        if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-low"))) {       # this is to make sure that GFP-low gate exists!
          getData_splitPeak(r = r, gs = r$gs, bin = "GFP-low", filter_name = "MyHC+")
        }})
    })
    gfp_medium_myo_high <- reactive({
      req(r$gs)
      withProgress(message = "Retrieving data from GFP-low bin...", {
        if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-medium"))) {        # this is to make sure that GFP-medium gate exists!
          getData_splitPeak(r = r, gs = r$gs, bin = "GFP-medium", filter_name = "MyHC+")
        }})
    })
    gfp_high_myo_high <- reactive({
      req(r$gs)
      withProgress(message = "Retrieving data from GFP-high bin...", {
        if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-high"))) {        # this is to make sure that GFP-high gate exists!
          getData_splitPeak(r = r, gs = r$gs, bin = "GFP-high", filter_name = "MyHC+")
        }})
    })

# Making a reactive expression gate_myosin_plot() to use inside the geom_gate() argument of the final ggcyto plot
### for each GFP bin, we have a separate list of gates (gfp_low_myo_high(), etc.) and in EACH of these lists we have individual gates for each dataset
### when the user wants to see the splitted peaks in the GFP-medium bin, the geom_gate argument of the ggcyto plot should also call the correct list of gates. We therefore let the user select the GFP bin he wants to see using a selectInput in UI. Depending on the state of this input, we fetch the correct list of gates.
### ideally the controller only displays the possible options, e.g. if user adds "GFP-low" bin he can't select GFP-medium on the controller

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

# Making a reactive value stat captures the state of the observer that adds the MyHC cutoffs to the gatingSet (by applying the add_gate custom function)
observer.state.split <- reactiveVal(FALSE)

# Making an observer that adds the MyHC cutoffs to the gatingSet (by applying the add_gate custom function)
### recompute(r$gs) already wrapped inside add_gate() custom function
### Note: plot(r$gs) doesn't work properly from now on. I guess thatit only plots the gates that are common across all samples. However, we will add gates to individual samples using add_gate() as the sample names in our gatingSet do not necessarily match those in the gates we add (because for some samples to gate could be determined due to low number of events, and these NULLs had to be removed. Same problem with gs_get_pop_paths(): workaround using the reactive expression final_output().

### This addition to the gatingSet is under the control of input$split: document this properly in the mainPanel and README

observe({
    if (!is.null(gfp_low_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_low_myo_high(), parent = "GFP-low")}
    if (!is.null(gfp_medium_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_medium_myo_high(), parent = "GFP-medium")}
    if (!is.null(gfp_high_myo_high())) {add_gate(r = r, gs = r$gs, gate = gfp_high_myo_high(), parent = "GFP-high")}
  observer.state.split(TRUE)
  updateTabsetPanel(inputId = "tabset-mainPanel", selected = "Plot")
}) |> bindEvent(input$split)

# Defining the output$myosin_splittedPeaks plot which shows the splitted peaks for the GFP bin chosen by the user (using the input$controller)
### Code only executed if observer.state.split() evaluates to TRUE, so only after the crucial observer that adds the gates has been executed
### Still under the control of input$split so that when the user changes some settings after having clicked split, nothing updates until he resets and clicks input$split again
output$myosin_splittedPeaks <- renderPlot({
  req(r$gs, selected_rows(), gate_myosin_plot(), input$controller)
  if (observer.state.split() == TRUE) {
    withProgress(message = "Plotting your data...", 
                 plot_myosin_splittedPeaks(r = r, gs = r$gs[[selected_rows()]], density_fill = "pink", gate = gate_myosin_plot(), subset = input$controller))
  }
}, res = 120)

# # Making a reactive expression that 
# flowSet_pData <- reactive({
#   req(r$fs)
#   pData(r$fs)
# })

# Defining the output$individual_FCS table which shows the individual datasets of the uploaded FCS file and let's users select the one they want to display
### The data displayed is r$flowSet_pData (shared from import module)
### Under the control of input$split because only then the user needs to be able to select it (showing it in the GFP bins tab might lead to confusion)
output$individual_FCS <- renderDT({
  req(r$flowSet_pData)
  r$flowSet_pData
  }, 
  selection = list(target = "row", selected = 1, mode = "multiple"), 
  rownames = FALSE, class = "cell-border stripe", 
  options = list(paging = FALSE, scrollY = "200px")) |> bindEvent(input$split)

# Making a reactive expression selected_rows() to capture the selected dataset in the table output$individual_FCS
selected_rows <- reactive(input$individual_FCS_rows_selected)

# Defining the reset processes
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
  observer.state.bins(FALSE) #to get the correct order of execution again when the user now clicks input$confirm again
  observer.state.split(FALSE) #to get the correct order of execution again when the user now clicks input$split again
}) |> bindEvent(input$confirm_bin_reset)

# ### this has to be modified!!!! see issue #13
# observe({
#   if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-low"))) {gs_pop_remove(r$gs, "GFP-low")} 
#   if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-medium"))) {gs_pop_remove(r$gs, "GFP-medium")}
#   if (any(str_detect(gs_get_pop_paths(r$gs), "GFP-high"))) {gs_pop_remove(r$gs, "GFP-high")}
#   for (i in seq_along(gates())) {
#     gs_pop_add(r$gs, gates()[[i]], parent = "GFP+")
#   }
#   recompute(r$gs)
#   #plot(r$gs)
# }) |> bindEvent(input$confirm_gate_reset)

# Defining all the modals needed to inform users on what's happening and guide them through the gating process
### help button modal
modal_help_button <- modalDialog(
  p("Select the GFP bin for which you want to display the MyHC intensitiy distribution."))

### confirm bins modal
modal_confirm_bins <- modalDialog(
  p("Your bins have been successfully added! Click ", span("Split", style = "color:#008cba; font-weight:bold"), " to split your MyHC peaks."),
  title = "Done!")

### confirm bin reset modal
modal_confirm_bin_reset <- modalDialog(
  "Are you sure you want to continue?",
  title = "Deleting your GFP bins",
  footer = tagList(
    actionButton(ns("cancel_bin_reset"), "Cancel"), 
    actionButton(ns("confirm_bin_reset"), "Delete", class = "btn btn-danger")))

### confirm gates reset modal
# modal_confirm_gates_reset <- modalDialog(
#   "Are you sure you want to continue?",
#   title = "Deleting your gates",
#   footer = tagList(
#     actionButton(ns("cancel_gate_reset"), "Cancel"),
#     actionButton(ns("confirm_gate_reset"), "Delete", class = "btn btn-danger")))

# observers to trigger the respective modals

# observe({
#   showModal(modal_confirm_bins)
#   updateTabsetPanel(inputId = "tabset", selected = "Split peaks")
# }) |> bindEvent(input$confirm_bins)


# observe({
#   updateTabsetPanel(inputId = "tabset-mainPanel", selected = "Plot")
# }) |> bindEvent(input$split)

### modal_help_button
observe({
  showModal(modal_help_button)
}) |> bindEvent(input$help)

### modal_confirm_bin_reset
#### this modal sets up two new buttons: input$cancel_bin_reset and input$confirm_bin_reset
####input$cancel_bin_reset removes the modal
observe({
  showModal(modal_confirm_bin_reset)
}) |> bindEvent(input$reset_bins)

observe({
  removeModal()
}) |> bindEvent(input$cancel_bin_reset)

### modal_confirm_gates_reset
#### this modal sets up two new buttons: input$cancel_gate_reset and input$confirm_bin_reset
# observe({
#   showModal(modal_confirm_gates_reset)
# }) |> bindEvent(input$reset_gates)
# 
# observe({
#   removeModal()
# }) |> bindEvent(input$cancel_gate_reset)

# Conditional visibility of buttons
### "Add_bins" button
observe({
  if (is.null(r$lower_limit_gfp)) {
    shinyjs::hide(id = "add_bins")
    }
  else {
    shinyjs::show(id = "add_bins")
    }
})

# "Confirm" and "Add GFP bins" buttons
observe({
  shinyjs::hide(id = "confirm_bins")
  shinyjs::hide(id = "add_bins")
}) |> bindEvent(input$confirm_bins)

observe({
  # removeUI(selector = "div:has(> #gate_1-gfp_bin_1)")
  # removeUI(selector = "div:has(> #gate_1-gfp_bin_2)")
  # removeUI(selector = "div:has(> #gate_1-gfp_bin_3)")
  shinyjs::show(id = "confirm_bins")
  # shinyjs::show(id = "add_bins")
  removeModal() # the modal_confirm_bin_reset is removed
  showNotification("Bins were successfully reset.")
  clicks$count <- 0 # reset clicks$count to 0;input$add_bins button should become visible again
}) |> bindEvent(input$confirm_bin_reset)

# "Gate" button
observe({
  shinyjs::hide(id = "split")
  shinyjs::show(id = "reset_gates")
}) |> bindEvent(input$split)


# observe({
#   shinyjs::hide(id = "reset_gates")
#   shinyjs::show(id = "split")
#   showNotification("Gates were successfully reset.")
#   removeModal()
# }) |> bindEvent(input$confirm_gate_reset)

  })}





# CUSTOM FUNCTIONS --------------------------------------------------------
render_bin_UI <- function(bin_number, value, ns, label, output , r) {
  output[[paste0("gfp_bin_", bin_number)]] <- renderUI(numericRangeInput(ns(paste0("gfp_range_", bin_number)), label = paste0("GFP ", label), value = value))
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
