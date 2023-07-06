#' curate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter autoWaiter

mod_curate_ui <- function(id){
  ns <- NS(id)
  useShinyjs()

  # Defining a tabPanel layout ----------------------------------------------
  tabPanel(title = "Curate",

           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             # Defining the sidebarPanel
             sidebarPanel(
               # Defining the input selections ------------------
               ### uiOutput corresponds to all the selections the user has to make in order to curate his data. This is an uiOutput because the choices to make should not be pre      -                   defined but rather determined based on the available channels in the fcs file.
               uiOutput(ns("input_selection")), br(),
               # Defining action buttons to start and restart curation
               actionButton(ns("Curate"), "Start curation", class = "btn-primary"),
               actionButton(ns("Delete"), "Restart curation", class = "btn-warning")),

             # Defining the mainPanel
             mainPanel(
               tabsetPanel(
                 tabPanel("Information", icon = icon("info"),
                          h1(strong("How curation works.")),

                          div(
                            p("By curation we understand ", strong("two essential steps:")),
                            p("- Exclusion of cellular debris using a pre-defined gate", style = "text-indent: 25px"),
                            p("- Removal of background noise and signals from cellular autofluorescence in the GFP channel by applying an intensity threshold based on the underlying data.", style = "text-indent: 25px"),
                            p("On the left, select the correct channels and control samples. Click the ", span("Start curation", style = "color:#008cba; font-weight:bold"), " button to start the curation.", br(), strong("Note:"), " In case you started curation with the wrong channel/sample selections, click", span("Restart curation", style = "color:#e99003; font-weight:bold"), " select the correct channels/samples, then click", span("Start curation", style = "color:#008cba; font-weight:bold"), " again."),
                            p("After curation is done, you can navigate between the two tabs at the top:"),
                            p("1) `NonDebris gate` displays the SSC vs FSC plot of your unlabeled control. The gate used to exclude debris is colored in red.", style = "text-indent: 25px"),
                            p("2) `GFP gate` displays a histogram of GFP intensities of your unlabeled control. The red line indicates the threshold used to remove unspecific GFP intensity values.", style = "text-indent: 25px"),

                            p("You can now switch to ", strong("'Gate'"), " in the menu bar."),
                            style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px")),

                 tabPanel("NonDebris gate",
                          br(),
                          # Defining the nonDebris gate plot that shows the user the results of the curation step that removed the debris -------------------------------------
                          plotOutput(ns("non_debris_gate"))),

                 tabPanel("GFP gate",
                          br(),
                          # Defining the gfp gate plot that shows the user the results of the curation step that sets a threshold between the GFP- and GFP+ population
                          plotOutput(ns("gfp_gate")),br(),
                          # Defining the text output that gives the numeric value of the GFP threshold
                          textOutput(ns("gfp_gate_numeric"))))),
           ))}

#' curate Server Functions
#'
#' @noRd
#' @importFrom purrr is_null
#' @import ggplot2
#' @importFrom ggcyto ggcyto geom_gate geom_stats scale_x_flowjo_biexp
#' @rawNamespace import(flowWorkspace, except = show)
#' @importFrom shinyjs show hide useShinyjs
#'
mod_curate_server <- function(id, r = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Computing the input_selection output: sidebar selections based on the channels contained in the uploaded fcs file -----------
    ### selectInput01() is a custom function: selection choices need to access r$gs/r$fs so a simple selectInput in the UI was not possible
    ### see the custom functions section at the end of the document for details
    output$input_selection <- renderUI({
      req(r$fs)
      tagList( #not sure if tagList necessary
        # channel selections
        selectInput01("forward_scatter", "Forward Scatter", n = 1, r = r, ns = ns),
        selectInput01("side_scatter", "Side Scatter", n = 2, r = r, ns = ns),
        selectInput01("kras_channel", "GFP channel", n = 3, r = r, ns = ns),
        selectInput01("myhc_channel", "MyHC channel", n = 6, r = r, ns = ns),
        # control sample selections
        selectInput01("negative_control", "double-negative control", n = 1, r = r, row = TRUE, ns = ns),
        selectInput01("positive_control_myhc", "MyHC+ control", n = 3, r = r, row = TRUE, ns = ns))
    })

    # Making a reactive expression (here a list) of the different user inputs
    input_list <- reactive({
      list(input$forward_scatter, input$side_scatter, input$kras_channel, input$myhc_channel, input$negative_control, input$positive_control_myhc)
    })

    # Making the nonDebris gate that excludes debris from the analysis.
    ### This is a hard-coded gate that always stays the same, for each sample and for each experiment.

    ### Defining the gate matrix: two columns, for the x-and y-coordinates of each corner of the gate boundary
    pgn_cut <- matrix(c(12500, 99000, 99000,0,0,6250, 6250, 99000, 99000,12500), ncol = 2, nrow = 5)

    ### Making a reactive expression gate_non_debris that changes the column names of the gate matrix according to the user inputs and then creates the polygonGate
    gate_non_debris <- reactive({
      req(r$gs)
      colnames(pgn_cut) <- c(input$side_scatter, input$forward_scatter)
      polygonGate(filterId = "NonDebris", .gate = pgn_cut)
    })

    # Making the GFP gate -----------------------------------
    ### Making a reactive expression lower_limit_gfp_gate() that applies the custom function get_lowerLimit(). This function applies a quantile gate to find a cutoff for which 99% of     the data lie below this cutoff. By applying such a gate to the GFP-channel and using the control samples that should not contain GFP signal, we get a good cutoff value separating     the GFP- from the GFP+ population
    ### we still need a bindEvent because we don't want it to execute if after curation some user still changes some settings
    ### get_lowerLimit() is a custom function: see end of document for details
    lower_limit_gfp_gate <- reactive({
      get_lowerLimit(gs = r$gs, datasets = c(input$negative_control, input$positive_control_myhc), node = "NonDebris", ch_gate = input$kras_channel, r = r)
    }) |> bindEvent(input$Curate)

    ### Making a reactive expression gfp_gate() that makes a gate out of the cutoff value determined above using lower_limit_gfp_gate
    ### ### we still need a bindEvent because we don't want it to execute if after curation some user still changes some settings
    ### make_gate() is a custom function: see end of document for details
    gfp_gate <- reactive({
      make_gate(lower_limit_gfp_gate(), input$kras_channel, filterId = "GFP+")
    }) |> bindEvent(input$Curate)

    ### Making a reactive value observer.state to track the state of the observer below. Plots should only be plotted once this observer has executed i.e once the value of this           reactive value evaluates to TRUE. Resetting should reset this reactive value back to FALSE.
    observer.state <- reactiveVal(FALSE)

    ### Add the created gateS to the gatingSet. This is an observer (because we need the side-effects of functions!) that depends on input$Curate.
    ### Since this observer is the only one that calls gfp_gate() and gate_non_debris(), these reactive expressions will only be evaluated when the "Curate" button has been clicked.
    ### Gate is added to r$gs (shared from import module)

    observe({
      gs_pop_add(r$gs, gate_non_debris(), parent = "root")
      message("Added the non_debris gate to the gatingSet")
      recompute(r$gs)
      message("Recomputed the gatingSet")
      gs_pop_add(r$gs, gfp_gate(), parent = "NonDebris")
      message("Added the gfp_gate to the gatingSet")
      recompute(r$gs)
      message("Recomputed the gatingSet")
      showModal(modal_finished)
      observer.state(TRUE)
      r$lower_limit_gfp <- lower_limit_gfp_gate()
      }) |> bindEvent(input$Curate, ignoreInit = TRUE)

    ### Computing the output plot that shows the user the result of the curation step that removes debris.
    ### The renderPlot() call does not need a bindEvent anymore. Default channel names are not taken because observer.state() == TRUE only when the observer above has been executed       and this observer depends on input$Curate.
    ### We still need a bindEvent(input$Curate) because we don't want the plot to update if the user changes settings AFTER clicking the curate button
    ### ggcyto data is r$gs (shared from import module)
    ### bindEvent(input$ok) to remove the plots when the user resets the curation
    output$non_debris_gate <- renderPlot({
      if (observer.state() == TRUE) {
      ggcyto(r$gs[[c(input$negative_control, input$positive_control_myhc)]],
             aes(x = .data[[input$side_scatter]] , y = .data[[input$forward_scatter]]),
             subset = "root") +
        geom_hex(bins = 150) +
        theme_bw() +
        geom_gate(gate_non_debris()) +
        geom_stats()
    }}, res = 120) |> bindEvent(input$Curate, input$ok)

    ### Compute the output plot that shows the user the result of the curation step that removes the GFP- population. Note that this plot is only executed when the crucial observer       that adds the NonDebris and GFP gate has been executed.
    ### We still need a bindEvent(input$Curate) because we don't want the plot to update if the user changes settings AFTER clicking the curate button
    ### bindEvent(input$ok) to remove the plots when the user resets the curation
      output$gfp_gate <- renderPlot({
        if (observer.state() == TRUE) {
        ggcyto(r$gs[[c(input$negative_control, input$positive_control_myhc)]],
               aes(x = .data[[input$kras_channel]]),
               subset = "NonDebris") +
          geom_histogram(bins = 50, fill = "palegreen1", color = "black") +
          theme_bw() +
          geom_gate(gfp_gate()) +
          scale_x_flowjo_biexp()
      }}, res = 120) |> bindEvent(input$Curate, input$ok)

      ### Compute the output text that informs the user on the value of the GPF cutoff and its interpretation. Note that this text is only executed when the crucial observer that adds       the NonDebris and GFP gate has been executed
      ### here we don't need a bindEvent(input$Curate) because lower_limit_gfp_gate is under the control of one (this is different from above where in the renderPlot call input values       are called directly)
      output$gfp_gate_numeric <- renderText({
        if (observer.state() == TRUE) {
          paste0("The GFP-cutoff separating your GFP- and GFP+ population is ", round(lower_limit_gfp_gate(), digits = 1), ". Thus, cells with GFP intensities below this value are excluded from your analysis as they most likely correspond to untransfected, autofluorescent cells.")
        }
      })

      # modals to guide the user through the app
      ### confirm reset modal
      modal_confirm <- modalDialog(
        "Are you sure you want to continue?",
        title = "Resetting your curation.",
        footer = tagList(
          actionButton(ns("cancel"), "Cancel"),
          actionButton(ns("ok"), "Restart", class = "btn btn-danger")))

      #### show the modal when input$Delete is clicked
      observe({
        showModal(modal_confirm)
      }) |> bindEvent(input$Delete)

      #### remove the modal when input$ok or input$cancel is clicked
      observe({
        showNotification("Curation reset")
        removeModal()
      }) |> bindEvent(input$ok)

      ### job finished modal
      modal_finished <- modalDialog(
        p("Your data has been successfully curated. Click on the ", strong("`NonDebris`"), " or ", strong("`Gate`"), " tabs to see the results." , br(), br(), "Select ", strong("`Gate`"), " in the menu bar to proceed your analysis.", br(), br(), "Click ", span("Restart curation", style = "color:#e99003; font-weight:bold"), "to select different channels/control samples and restart your curation."),
        title = "Done!")

      #### remove modal when input$cancel is clicked
      observe({
        removeModal()
      }) |> bindEvent(input$cancel)

      #### show modal when input$Curate is clicked (included in observer higher above)

    # Conditionsl visibility of buttons
    ### Hide the Curate button when input$Curate is clicked
    observe({
      shinyjs::hide(id = "Curate")
    }) |> bindEvent(input$Curate)

    ### Show the Curate button when input$ok is clicked (input$ok button appears when users reset the curation)
    observe({
      shinyjs::show(id = "Curate")
    }) |> bindEvent(input$ok)


    # alterts if non-unique inputs
    ### (1) initially, return NULL if any of the inputs are NULL (inputs non existant)
    ### (2) then check for duplications, anyDuplicated() returns :
    ###     - index i of first duplicated entry x[i] if there are any
    ###     -  0 otherwise --> > 0 is therefore a good condition.

    observe({
      if (any(sapply(input_list(), is.null))) return(NULL)
      else if (anyDuplicated(input_list()) > 0) {
        showModal(modalDialog("Inputs have to be unique.", title = "Warning.",
                              footer = modalButton("Dismiss")))
      }})

    # strategie du petit r to share variables across modules
    r$fsc <- reactive(input$forward_scatter)
    r$ssc <- reactive(input$side_scatter)
    r$ch_kras <- reactive(input$kras_channel)
    r$ch_myhc <- reactive(input$myhc_channel)

    r$ctrl_myhc <- reactive(input$positive_control_myhc)
    r$ctrl_negative <- reactive(input$negative_control)

    # Reset the curation
    ### create new gs from initial fs and reset the observer.state back to FALSE when the user clicks input$ok
    observe({
      r$gs <- GatingSet(r$fs)
      observer.state(FALSE)
    }) |> bindEvent(input$ok)

  })}

#' @importFrom openCyto gate_quantile
#' @rawNamespace import(flowCore, except = show)

### create_quantile_gate:
create_quantile_gate <- function(samples, gate_channel) {
  #require(flowCore)
  fsApply(samples, function(fr) {
    openCyto::gate_quantile(fr, channel = gate_channel, probs = 0.99)
  })
}

### get_lowerLimit:
# - gs: the gatingSet you want to extract data from
# - datasets: the dataSets you want to extract data from
# - node: the node/gate inside the gatingSet you want to extract data from
# - ch_gate: the name of the channel you want to perform a quantileGate on
get_lowerLimit <- function(gs, datasets, node, ch_gate, r) {
  # extract the data as a flowSet
  x <- gs_pop_get_data(r$gs[[datasets]], node) |> cytoset_to_flowSet()
  # create a quantile gate using extracted data and the respective channel name
  y <- create_quantile_gate(x, gate_channel = ch_gate)
  # average the minimum values from the respective quantile gateS(!)
  z <- mean(c(y[[1]]@min, y[[2]]@min))
}

### make_gate:
make_gate <- function(lower_limit, col_name, filterId) {
  mat <- matrix(c(lower_limit, Inf), ncol = 1)
  colnames(mat) <- col_name
  return(rectangleGate(filterId = filterId, .gate = mat))
}

#selectInput01:
#row argument:
#- TRUE for ROWnames in selectInput's "choices" argument
# - FALSE (default) for COLnames in selectInput's "choices" argument
selectInput01 <- function(id, label, n, r, row = FALSE, ns) {
  if (row == FALSE) {
    selectInput(ns(id), label = label, choices = c("", colnames(r$fs)), selected = colnames(r$fs)[n])
  }
  else {
    selectInput(ns(id), label = label, choices = c("", rownames(pData(r$fs))), rownames(pData(r$fs))[n])
  }
}


## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
