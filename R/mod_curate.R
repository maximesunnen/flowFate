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
               uiOutput(ns("input_selection"))
             ),

             mainPanel(

               # header and text description of curation ---------------------------------
               h1("How curation works."),
               p("By curation we understand two essential steps.
                    First, we want to focus our analysis on intact
                    cells and not debris. We therefore need to set
                    a gate that excludes cellular debris,
                    which normally clusters in the lower left corner
                    in a SSC vs FSC plot. Second, we have to define
                    intensity thresholds in our fluorescent channels
                    below which we cannot distinguish between a real signal
                    and autofluorescence/background noise. We will define
                    both the non-debris gate and the threshold using our controls"),

               # Action button to start curation
               #uiOutput(ns("conditional_curate_button")),
               actionButton(ns("Curate"), "Start curation"),
               actionButton(ns("Delete"), "Restart curation"),

               # plot SSC vs FSC for control samples -------------------------------------
               #plotOutput(ns("controls_ssc_fsc")),
               plotOutput(ns("non_debris_gate")),

               plotOutput(ns("gfp_gate")),

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
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

observe({
  shinyjs::hide(id = "Curate")
}) |> bindEvent(input$Curate)
#
observe({
  shinyjs::show(id = "Curate")
}) |> bindEvent(input$ok)


    #selectInput custom function
    selectInput01 <- function(id, label, n, r, row = FALSE) {
      if (row == FALSE) {
        selectInput(ns(id), label = label, choices = c("", colnames(r$fs)), selected = colnames(r$fs)[n])
      }
      else {
        selectInput(ns(id), label = label, choices = c("", rownames(pData(r$fs))), rownames(pData(r$fs))[n])
      }
    }

# modal to restart curation -----------------------------------------------

    modal_confirm <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting files",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Restart", class = "btn btn-danger")
      )
    )

    # All sidebar selections/inputs (control datasets and channels) -----------

    output$input_selection <- renderUI({
      req(r$fs)

      tagList(
        selectInput01("forward_scatter", "Forward Scatter", n = 1, r = r),
        selectInput01("side_scatter", "Side Scatter", n = 2, r = r),
        selectInput01("kras_channel", "KRas channel", n = 3, r = r),
        selectInput01("myhc_channel", "Myosin channel", n = 6, r = r),

        selectInput01("negative_control", "Negative control", n = 1, r = r, row = TRUE),
        selectInput01("positive_control_kras", "Positive control (KRas)", n = 2, r = r, row = TRUE),
        selectInput01("positive_control_myhc", "Positive control (MYHC)", n = 3, r = r, row = TRUE)
      )
    })

    # alerts if non-unique channels/control datasets --------------------

    fsc <- reactive(input$forward_scatter)
    ssc <- reactive(input$side_scatter)
    ch_kras <- reactive(input$kras_channel)
    ch_myhc <- reactive(input$myhc_channel)

    ctrl_kras <- reactive(input$positive_control_kras)
    ctrl_myhc <- reactive(input$positive_control_myhc)
    ctrl_negative <- reactive(input$negative_control)

    observe({
      input_list <- list(fsc(), ssc(), ch_kras(), ch_myhc(), ctrl_negative(),ctrl_kras(), ctrl_myhc())

      if (any(sapply(input_list, is.null))) return(NULL)

      else if (anyDuplicated(input_list) > 0) {
        showModal(modalDialog("Inputs have to be unique.", title = "Warning.",
          footer = modalButton("Dismiss")))
      }
    })

    observe({
      r$gs <- GatingSet(r$fs)
    }) |> bindEvent(input$ok)

    # SSC vs FSC plot of control samples --------------------------------------
    # We need a set of reactive expressions that capture the input from our selectInput widgets defined above. Since we do not want the app to perform computations every time the input changes - but rather when the user is "finished" defining his inputs - put these reactive expressions under the control of a new button "Curate" (accessed by input$Curate). Code depending on e.g. control_indices() should therefore also not update unless "Curate" is clicked (results are cached, and before input$Curate, this code would not know that the input has changed).

    # Question: with the code below, you create a reactive expression with a dependency on input$Curate. BUT, when input$Curate changes (e.g the user clicks on the button), the entire code is computed, regardless if its result has changed or not? Here this is not a problem because accessing the inputs is not computationally expensive, but we should keep this in mind.

    control_indices <- eventReactive(input$Curate, {
      c(ctrl_kras(), ctrl_myhc(), ctrl_negative())
    })

    # We now need to define a reactive expression generating our first gate. How this gate is generated obviously depends on the name of our side_scatter and forward_scatter reactive expressions (i.e which channels the user wants to gate on). These reactive expressions have a dependency on input$Curate, so they won't change unless the user clicks "Curate".

    # Question 1: The code below create a reactive expression that creates the first gate. I don't know why this does not give an error of the type "can't find function side_scatter()", because side_scatter does not exist before the user clicks "Curate".

    pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),
                      ncol = 2,
                      nrow = 5)

    observe({
        colnames(pgn_cut) <- c(ssc(), fsc())
        message("Renamed the columns of pgn_cut")

        # create the gate using flowCore's polygonGate
        gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
        message("Created the gate")

        if (is.null(gate_non_debris)) return(NULL)

        gs_pop_add(r$gs, gate_non_debris, parent = "root")
        message("Added the non_debris gate to the gatingSet")

        # recompute the GatingSet: performs calculations
        recompute(r$gs)
        message("Recomputed the gatingSet")

        #plot the gate
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


observe({
      # Curate background noise: KRas channel -----------------------------------

      # extract NonDebris population data, change object type to flowSet
      nonDebris_data <- gs_pop_get_data(r$gs[[c(ctrl_negative(),ctrl_myhc())]],
                                        "NonDebris") |> cytoset_to_flowSet()
      message("nonDebris_data created and changed to flowSet")

      # create a quantileGate for both controls: creates a list of two gates
      gfp_test_gate <- create_quantile_gate(nonDebris_data, gate_channel = ch_kras())
      message("gfp_test_gate created")
      print(gfp_test_gate)

      # average the lower boundary from both gates: use list accessors
      lower_limit_gfp_gate <- mean(c(gfp_test_gate[[1]]@min, gfp_test_gate[[2]]@min))
      message("averaged the gfp gate values")

      # create the final gfp gate
      ## had to do a workaround because of annoying parse( ) error!
      mat <- matrix(c(lower_limit_gfp_gate, Inf), ncol = 1)
      colnames(mat) <- ch_kras()
      gfp_gate <- rectangleGate(filterId = "GFP+",
                                .gate = mat)

      print(gfp_gate)
      message("GFP gate created")

      # add gate to the gatingSet: parent should be NonDebris
      gs_pop_add(r$gs, gfp_gate, parent = "NonDebris")
      message("Added gfp_gate to the gatingSet")

      # recompute the GatingSet
      recompute(r$gs)
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

observe({

      # Curate background noise: MYHC channel -----------------------------------

      # extract NonDebris population data, change object type to flowSet
      nonDebris_data <- gs_pop_get_data(r$gs[[control_indices()[c(2,3)]]], "NonDebris") |>
        cytoset_to_flowSet()
      message("nonDebris_data created and changed to flowSet")

      # create a quantileGate for both controls: creates a list of two gates
      myhc_test_gate <- create_quantile_gate(nonDebris_data, gate_channel = input$myhc_channel)
      print(myhc_test_gate)
      message("myhc_test_gate created")

      # average the lower threshold from both gates
      lower_limit_myhc_gate <- mean(c(myhc_test_gate[[1]]@min, myhc_test_gate[[2]]@min))
      message("averaged the myhc gate values")

      # create the final myhc gate
      ## had to do a workaround because of annoying parse( ) error!
      mat <- matrix(c(lower_limit_myhc_gate, Inf), ncol = 1)
      colnames(mat) <- input$myhc_channel
      myhc_gate <- rectangleGate(filterId = "MYO+",
                                .gate = mat)

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
    # create reactive dependency of observe() on input$Curate

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



## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
