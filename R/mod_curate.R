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
             sidebarPanel(
               # Input selections for used channels and control samples ------------------
               uiOutput(ns("input_selection")),
             br(),
             # Action button to start curation
             actionButton(ns("Curate"), "Start curation", class = "btn-primary"),
             actionButton(ns("Delete"), "Restart curation", class = "btn-warning")),

             mainPanel(
               h1(strong("How curation works.")),
               div(p("By curation we understand ", strong("two essential steps:")),
                   p("- First, our analysis should be focused on intact cells. We exclude cellular debris – clustering on the lower left corner on an SSC vs FSC plot – by applying a rectangular gate.", style = "text-indent: 25px"),
                   p("-	Second, FACS data is never devoid of background noise or signals from cellular autofluorescence. By applying intensity thresholds on our fluorescent channels, we essentially get rid of these undesirable signals.", style = "text-indent: 25px"),

                   p("We will use a ", em("pre-defined"), " rectangle gate to remove debris while our controls will help us get rid of unspecific signals."), style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px"), br(),

                   div(p("On the left, select the correct channels and control samples. Click the ", span("Start curation", style = "color:#008cba; font-weight:bold"), " button to start the curation. If you later notice that you’ve selected the wrong channels and/or control samples, click the ", span("Restart curation", style = "color:#e99003; font-weight:bold"), " button and restart the procedure. You will have to click the ", span("Start curation", style = "color:#008cba; font-weight:bold"), " button again."),

                   p("After curation is done, three plots will appear:"),
                   p("1) SSC vs FSC with the rectangle gate used to remove debris (in red)", style = "text-indent: 25px"),
                   p("2) Intensity (GFP) vs density plot for your unlabelled and myosin control with the intensity threshold (in red)", style = "text-indent: 25px"),
                   p("3) Intensity (Myosin) vs density plot for your unlabelled and GFP control with the intensity threshold (in red)", style = "text-indent: 25px"),

                   p("The reason why you see two plots for the intensity thresholds is that we compute a quantile gate for ", strong("two"), " control samples and average the results. The red lines correspond to this ", strong("averaged"), " value."),

                   p("Once you have curated your data, you can proceed with to gating. Simply click on the ", strong("Gate"), " tab at the top of the page."),
                   style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px"),
               br(),
               # plot SSC vs FSC for control samples -------------------------------------
               uiOutput(ns("header_non_debris_gate")),
               hr(),
               br(),
               plotOutput(ns("non_debris_gate")),
               br(),
               br(),
               uiOutput(ns("header_gfp_gate")),
               hr(),
               br(),
               plotOutput(ns("gfp_gate")),
               br(),
               br(),
               uiOutput(ns("header_myhc_gate")),
               hr(),
               br(),
               plotOutput(ns("myhc_gate"))
             )))}

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


    observe({
      output$header_non_debris_gate <- renderUI({h3(strong("Your nonDebris gate"), style = "color:#008cba")})
        output$header_gfp_gate <- renderUI({h3(strong("Your GFP noise threshold"), style = "color:#008cba")})
        output$header_myhc_gate <- renderUI({h3(strong("Your MYHC noise threshold"), style = "color:#008cba")})
    }) |> bindEvent(input$Curate)

    # Make the Curate button conditionally visible
    observe({
      shinyjs::hide(id = "Curate")
    }) |> bindEvent(input$Curate)

    observe({
      shinyjs::show(id = "Curate")
    }) |> bindEvent(input$ok)

    # Restart curation MODAL
    modal_confirm <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting files",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Restart", class = "btn btn-danger")
      )
    )

    # Sidebar selections/inputs (control datasets and channels) -----------
    output$input_selection <- renderUI({
      req(r$fs)
      tagList(
        selectInput01("forward_scatter", "Forward Scatter", n = 1, r = r, ns = ns),
        selectInput01("side_scatter", "Side Scatter", n = 2, r = r, ns = ns),
        selectInput01("kras_channel", "GFP channel", n = 3, r = r, ns = ns),
        selectInput01("myhc_channel", "Myosin channel", n = 6, r = r, ns = ns),
        selectInput01("negative_control", "Unstained control", n = 1, r = r, row = TRUE, ns = ns),
        selectInput01("positive_control_kras", "GFP-positive control", n = 2, r = r, row = TRUE, ns = ns),
        selectInput01("positive_control_myhc", "Myosin-positive control", n = 3, r = r, row = TRUE, ns = ns)
      )
    })

    # Alerts if non-unique channels/control datasets
    ## create reactive expressions to avoid typing input$XXX every time
    ### in fact, assigning only to r$XXX is sufficient, but then need to change this everywhere e.g. fsc() was used to r$fsc()
    fsc <- reactive(input$forward_scatter)
    r$fsc <- reactive(input$forward_scatter)

    ssc <- reactive(input$side_scatter)
    r$ssc <- reactive(input$side_scatter)

    ch_kras <- reactive(input$kras_channel)
    r$ch_kras <- reactive(input$kras_channel)

    ch_myhc <- reactive(input$myhc_channel)
    r$ch_myhc <- reactive(input$myhc_channel)

    ctrl_kras <- reactive(input$positive_control_kras)
    r$ctrl_kras <- reactive(input$positive_control_kras)

    ctrl_myhc <- reactive(input$positive_control_myhc)
    r$ctrl_myhc <- reactive(input$positive_control_myhc)

    ctrl_negative <- reactive(input$negative_control)
    r$ctrl_negative <- reactive(input$negative_control)

    observe({
      input_list <- list(fsc(), ssc(), ch_kras(), ch_myhc(),
                         ctrl_negative(),ctrl_kras(), ctrl_myhc())
      if (any(sapply(input_list, is.null))) return(NULL)
      else if (anyDuplicated(input_list) > 0) {
        showModal(modalDialog("Inputs have to be unique.", title = "Warning.",
          footer = modalButton("Dismiss")))
      }
    })
  # Comments on the function above:
  # (1) initially, return NULL if any of the inputs are NULL (inputs inexistant)
  # (2) then check for duplications, anyDuplicated() returns :
  #     - index i of first duplicated entry x[i] if there are any
  #     -  0 otherwise --> > 0 is therefore a good condition.

    # if user wants to reset curation (input$ok), create new gs from initial fs
    observe({
      r$gs <- GatingSet(r$fs)
    }) |> bindEvent(input$ok)

    # SSC vs FSC plot of control samples --------------------------------------
    # We need a set of reactive expressions that capture the input from our selectInput widgets defined above. Since we do not want the app to perform computations every time the input changes - but rather when the user is "finished" defining his inputs - put these reactive expressions under the control of a new button "Curate" (accessed by input$Curate). Code depending on e.g. control_indices() should therefore also not update unless "Curate" is clicked (results are cached, and before input$Curate, this code would not know that the input has changed).

    # Question: with the code below, you create a reactive expression with a dependency on input$Curate. BUT, when input$Curate changes (e.g the user clicks on the button), the entire code is computed (correct?), if the results have changed or not? Here this is not a problem because accessing the inputs is not computationally expensive, but we should keep this in mind. isolate() is what you need here!

    # We now need to define a reactive expression generating our first gate. How this gate is generated obviously depends on the name of our side_scatter and forward_scatter reactive expressions (i.e which channels the user wants to gate on). These reactive expressions have a dependency on input$Curate, so they won't change unless the user clicks "Curate".

    # Question 1: The code below create a reactive expression that creates the first gate. I don't know why this does not give an error of the type "can't find function side_scatter()", because side_scatter does not exist before the user clicks "Curate".

    observe({
      pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),
                        ncol = 2,
                        nrow = 5)
      colnames(pgn_cut) <- c(ssc(), fsc())
      message("Renamed the columns of pgn_cut")
      # create a polygonGate
      gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
      message("Created the gate")

      if (is.null(gate_non_debris)) return(NULL)
      gs_pop_add(r$gs, gate_non_debris, parent = "root")
      message("Added the non_debris gate to the gatingSet")

      recompute(r$gs)
      message("Recomputed the gatingSet")

      output$non_debris_gate <- renderPlot({
        ggcyto(isolate(r$gs[[c(ctrl_negative(),ctrl_kras(),ctrl_myhc())]]),
               aes(x = .data[[ssc()]] , y = .data[[fsc()]]),
               subset = "root") +
          geom_hex(bins = 150) +
          theme_bw() +
          geom_gate(gate_non_debris) +
          geom_stats()
      })
    }) |> bindEvent(input$Curate, ignoreInit = TRUE)

    # Curate background noise: KRas channel -----------------------------------
    observe({
      # custom function: see end of document for details
      lower_limit_gfp_gate <- get_lowerLimit(gs = r$gs,
                                             datasets = c(ctrl_negative(), ctrl_myhc()),
                                             node = "NonDebris",
                                             ch_gate = ch_kras(),
                                             r = r)
      # For testing/debugging
      message("lower_limit_gfp_gate successfully computed")
      r$lower_limit_gfp <- lower_limit_gfp_gate
      print(r$lower_limit_gfp)

      # create the final gfp gate
      gfp_gate <- make_gate(lower_limit_gfp_gate, ch_kras(),filterId = "GFP+")

      # For testing/debugging
      print(gfp_gate)
      message("GFP gate created")

      # add gate to the gatingSet: parent should be NonDebris
      gs_pop_add(r$gs, gfp_gate, parent = "NonDebris")
      # For testing/debugging
      message("Added gfp_gate to the gatingSet")

      # recompute the GatingSet
      recompute(r$gs)
      # For testing/debugging
      message("Recomputed the gatingSet")

      # plot the gate
      output$gfp_gate <- renderPlot({
        ggcyto(isolate(r$gs[[c(ctrl_negative(),ctrl_myhc())]]),
               aes(x = .data[[ch_kras()]]),
               subset = "NonDebris") +
          geom_density(fill = "forestgreen") +
          theme_bw() +
          geom_gate(gfp_gate) +
          geom_stats() +
          scale_x_flowjo_biexp()
      })}) |> bindEvent(input$Curate, ignoreInit = TRUE)

    # Curate background noise: MYHC channel -----------------------------------
    observe({
      # custom function: see end of document for details
      lower_limit_myhc_gate <- get_lowerLimit(gs = r$gs,
                                             datasets = c(ctrl_negative(), ctrl_kras()),
                                             node = "NonDebris",
                                             ch_gate = ch_myhc(),
                                             r = r)

      print(lower_limit_myhc_gate)
      r$lower_limit_myhc <- lower_limit_myhc_gate

      # create the final myhc gate
      myhc_gate <- make_gate(lower_limit_myhc_gate, ch_myhc(),filterId = "MYO+")

      print(myhc_gate)
      message("MYHC gate created")

      # add gate to the gatingSet: parent should be GFP+ (!)
      gs_pop_add(r$gs, myhc_gate, parent = "GFP+")
      message("Added myhc_gate to the gatingSet")

      # recompute the GatingSet
      recompute(r$gs)
      message("Recomputed the gatingSet")

      # plot the gate
      output$myhc_gate <- renderPlot({
        ggcyto(isolate(r$gs[[c(ctrl_negative(),ctrl_kras())]]),
               aes(x = .data[[ch_myhc()]]),
               subset = "NonDebris") +
          geom_density(fill = "pink") +
          theme_bw() +
          geom_gate(myhc_gate) +
          geom_stats() +
          scale_x_flowjo_biexp()
      })
    }) |> bindEvent(input$Curate, ignoreInit = TRUE)

    observe({
      showModal(modal_confirm)
    }) |> bindEvent(input$Delete)

    observe({
      showNotification("Curation reset")
      removeModal()
    }) |> bindEvent(input$ok)

    observe({
      removeModal()
    }) |> bindEvent(input$cancel)

  })}

#' @importFrom openCyto gate_quantile
#' @rawNamespace import(flowCore, except = show)

### create_quantile_gate:
create_quantile_gate <- function(samples, gate_channel) {
  require(flowCore)
  fsApply(samples,
          function(fr) {
            print(fr)
            openCyto::gate_quantile(fr,
                                    channel = gate_channel,
                                    probs = 0.99)
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
  # create a quantile get using extracted data and the respective channel name
  y <- create_quantile_gate(x, gate_channel = ch_gate)
  # average the minimum values from the respective quantile gateS(!)
  z <- mean(c(y[[1]]@min, y[[2]]@min))
}

### make_gate:
#
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
