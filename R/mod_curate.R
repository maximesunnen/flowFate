#' curate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets useSweetAlert sendSweetAlert

mod_curate_ui <- function(id){
  ns <- NS(id)
  useSweetAlert()

  # Defining a tabPanel layout ----------------------------------------------
  tabPanel(title = "Curate",

           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             sidebarPanel(

               # Input selections for used channels and control samples ------------------
               uiOutput(ns("channel_selection")),
               uiOutput(ns("control_selection")),
             ),

             mainPanel(

               # header and text description of curation ---------------------------------
               h1("How curation works."),
               HTML("<p>By curation we understand two essential steps.
                    First, we want to focus our analysis on intact
                    cells and not debris. We therefore need to set
                    a gate that excludes cellular debris,
                    which normally clusters in the lower left corner
                    in a SSC vs FSC plot. Second, we have to define
                    intensity thresholds in our fluorescent channels
                    below which we cannot distinguish between a real signal
                    and autofluorescence/background noise. We will define
                    both the non-debris gate and the threshold using our controls.</p>"),

               # Action button to start curation
               actionButton(ns("Curate"), "Start curation"),

               # plot SSC vs FSC for control samples -------------------------------------
               #plotOutput(ns("controls_ssc_fsc")),
               plotOutput(ns("non_debris_gate")),

               plotOutput(ns("gfp_gate"))
             )))}


#' curate Server Functions
#'
#' @noRd
#' @importFrom purrr is_null
#' @import ggplot2
#' @importFrom ggcyto ggcyto geom_gate geom_stats
#' @import flowWorkspace
#' @importFrom openCyto gate_quantile


mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # All sidebar selections/inputs (control datasets and channels) -------------
    output$channel_selection <- renderUI({
      req(r$gs)
      
      tagList(
      selectInput(ns("forward_scatter"),
                  "Forward Scatter",
                  choices = c("", colnames(r$gs)),
                  selected = colnames(r$gs)[1]),

      selectInput(ns("side_scatter"),
                  "Side Scatter",
                  choices = c("", colnames(r$gs)),
                  selected = colnames(r$gs)[2]),

      selectInput(ns("kras_channel"),
                  "KRas channel",
                  choices = c("",colnames(r$gs)),
                  selected = colnames(r$gs)[3]),

      selectInput(ns("myhc_channel"),
                  "Myosin channel",
                  choices = c("",colnames(r$gs)),
                  selected = colnames(r$gs)[6])
      )
    })

    output$control_selection <- renderUI({
      req(r$gs)
      tagList(
        selectInput(ns("negative_control"),
                    "Negative control",
                    choices = c("", rownames(pData(r$fs))),
                    selected = rownames(pData(r$fs))[1]),
        
        selectInput(ns("positive_control_kras"),
                    "Positive control (KRAS)",
                    choices = c("", rownames(pData(r$fs))),
                    selected = rownames(pData(r$fs))[2]),
        
        selectInput(ns("positive_control_myhc"),
                    "Positive control (MYHC)",
                    choices = c("", rownames(pData(r$fs))),
                    selected = rownames(pData(r$fs))[3])
      )
    })
    
    
# alerts if non-unique channels/control datasets --------------------    
    
    observeEvent(c(input$positive_control_kras, input$positive_control_myhc, input$negative_control), {
      if (input$positive_control_kras %in% c(input$positive_control_myhc, input$negative_control) | input$positive_control_myhc == input$negative_control) {
        sendSweetAlert(
          session = session,
          title = "Warning.",
          text = "Control datasets have to be unique!",
          type = "warning"
        )
      }})
    
    observeEvent(c(input$forward_scatter, 
                   input$side_scatter, 
                   input$kras_channel, 
                   input$myhc_channel), {
                     if (input$forward_scatter %in% c(input$side_scatter, 
                                                      input$kras_channel, 
                                                      input$myhc_channel) | 
                         input$side_scatter %in% c(input$kras_channel, 
                                                   input$myhc_channel) | 
                         input$kras_channel %in% input$myhc_channel) {
                       
                       sendSweetAlert(
                         session = session,
                         title = "Warning.",
                         text = "Channel names have to be unique!",
                         type = "warning"
                       )
                     }})
    
    
# SSC vs FSC plot of control samples --------------------------------------
# We need a set of reactive expressions that capture the input from our selectInput widgets defined above. Since we do not want the app to perform computations every time the input changes - but rather when the user is "finished" defining his inputs - put these reactive expressions under the control of a new button "Curate" (accessed by input$Curate). Code depending on e.g. control_indices() should therefore also not update unless "Curate" is clicked (results are cached, and before input$Curate, this code would not know that the input has changed). 

# Question: with the code below, you create a reactive expression with a dependency on input$Curate. BUT, when input$Curate changes (e.g the user clicks on the button), the entire code is computed, regardless if its result has changed or not? Here this is not a problem because accessing the inputs is not computationally expensive, but we should keep this in mind.

    control_indices <- eventReactive(input$Curate, {           
      ## is it really indices or names of the datasets?
      c(input$positive_control_kras,
        input$positive_control_myhc,
        input$negative_control)
    })

    side_scatter <- eventReactive(input$Curate, {input$side_scatter})
    forward_scatter <- eventReactive(input$Curate, {input$forward_scatter})

# We now need to define a reactive expression generating our first gate. How this gate is generated obviously depends on the name of our side_scatter and forward_scatter reactive expressions (i.e which channels the user wants to gate on). These reactive expressions have a dependency on input$Curate, so they won't change unless the user clicks "Curate". 

# Question 1: The code below create a reactive expression that creates the first gate. I don't know why this does not give an error of the type "can't find function side_scatter()", because side_scatter does not exist before the user clicks "Curate". 

observe({
  # create the gate
  pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),      # place outside the server function?
                    ncol = 2,
                    nrow = 5)
  colnames(pgn_cut) <- c(side_scatter(), forward_scatter())
  gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
  message("Created the gate")
  
  # add gate to gs
  gs_pop_add(r$gs, gate_non_debris, parent = "root")
  message("Added the non_debris gate to the gatingSet")
  
  # recompute the GatingSet
  recompute(r$gs)
  message("Recomputed the gatingSet")
  
  #plot the gate
  output$non_debris_gate <- renderPlot({
    ggcyto(r$gs[[control_indices()]],
           aes(x = .data[[side_scatter()]] , y = .data[[forward_scatter()]]),
           subset = "root") +
      geom_hex(bins = 150) +
      theme_bw() +
      geom_gate(gate_non_debris) +
      geom_stats()
})}) |> bindEvent(input$Curate, ignoreInit = TRUE)        # create reactive dependency of observe() on input$Curate



# # curate background noise - KRAS channel ----------------------------------
#
observe({
  #extract NonDebris population data and change to flowSet
  nonDebris_data <- gs_pop_get_data(r$gs[[control_indices()[1]]], "NonDebris") |> cytoset_to_flowSet()
  message("nonDebris_data created")
  
  # create a quantileGate for both samples
  gfp_test_gate <- fsApply(nonDebris_data,
                         function(fr) openCyto:::gate_quantile(fr,
                                                   channel = "GRN.B.HLin",
                                                   probs = 0.99))
  
  message("gfp_test_gate created")
  # # average the lower threshold from both gates
  # lower_limit_gfp_gate <- mean(c(gfp_test_gate[[1]]@min, gfp_test_gate[[2]]@min))
  # 
  # # create the final gate
  # gfp_gate <- rectangleGate(input$kras_channel = c(lower_limit_gfp_gate, Inf), 
  #                           filterId = "GFP+")
  # 
  # output$gfp_gate <- renderPlot({
  #   ggcyto(r$gs[[control_indices()]],
  #          aes(x = .data[[input$kras_channel]]),
  #          subset = "root") +
  #     geom_density(fill = "forestgreen") +
  #     theme_bw() +
  #     geom_gate(gfp_gate) +
  #     geom_stats()
    
}) |> bindEvent(input$Curate, ignoreInit = TRUE)
  })}




## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
