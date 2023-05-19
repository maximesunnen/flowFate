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
               tabsetPanel(
                 tabPanel("Information", icon = icon("info"),
               h1(strong("How curation works.")),
               
               div(
                 p("By curation we understand ", strong("two essential steps:")),
                 p("- Exclusion of cellular debris using a pre-defined gate", style = "text-indent: 25px"),
                 p("- Removal of background noise and signals from cellular autofluorescence in the GFP channel by applying an intensity threshold based on the underlying data.", style = "text-indent: 25px"),
                 p("On the left, select the correct channels and control samples. Click the ", span("Start curation", style = "color:#008cba; font-weight:bold"), " button to start the curation.", br(), strong("Note:"), " In case you started curation with the wrong channel/sample selections, click", span("Restart curation", style = "color:#e99003; font-weight:bold"), " select the correct channels/samples, then click", span("Start curation", style = "color:#008cba; font-weight:bold"), " again."),
                 p("After curation is done, you can navigate between the two tabs at the top:"),
                 p("1) ‘NonDebris gate‘ displays the SSC vs FSC plot of your unlabeled control. The gate used to exclude debris is colored in red.", style = "text-indent: 25px"),
                 p("2) ‘GFP gate‘ displays a histogram of GFP intensities of your unlabeled control. The red line indicates the threshold used to remove unspecific GFP intensity values.", style = "text-indent: 25px"),
                   
                   # p("3) Intensity (MyHC) vs density plot for your unlabelled and GFP control with the intensity threshold (in red)", style = "text-indent: 25px"),

                   # p("The reason why you see two plots for the intensity thresholds is that we compute a quantile gate for ", strong("two"), " control samples and average the results. The red lines correspond to this ", strong("averaged"), " value."),

               p("You can now switch to ", strong("‘Gate‘"), " in the menu bar."),
               style = "text-align:justify;color:black;background-color:#f8f8f8;padding:15px;border-radius:10px")),
               
               tabPanel("NonDebris gate",
               # plot SSC vs FSC for control samples -------------------------------------
               br(),
               plotOutput(ns("non_debris_gate"))),
               
               tabPanel("GFP gate",
               br(),
               plotOutput(ns("gfp_gate")),
               br(),
               textOutput(ns("gfp_gate_numeric"))))),
               # 
               # tabPanel("MyHC threshold",
               # br(),
               # plotOutput(ns("myhc_gate")))
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

    # Hide the Curate button when Curate is clicked
    observe({
      shinyjs::hide(id = "Curate")
    }) |> bindEvent(input$Curate)
    
    # Show the Curate button when ok is clicked (ok button appears when users reset the curation)
    observe({
      shinyjs::show(id = "Curate")
    }) |> bindEvent(input$ok)

    # Restart curation modal: show when users reset the curation
    modal_confirm <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting files",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Restart", class = "btn btn-danger") # resets the curation
      )
    )

    # Sidebar selections/inputs (control datasets and channels) -----------
    # selectInput01 is a custom function: in the selection I need to access r$gs/r$fs so a selectInput in the UI was not possible
    # see custom functions for details on this function
    output$input_selection <- renderUI({
      req(r$fs)
      tagList( #not sure if tagList necessary
        selectInput01("forward_scatter", "Forward Scatter", n = 1, r = r, ns = ns),
        selectInput01("side_scatter", "Side Scatter", n = 2, r = r, ns = ns),
        selectInput01("kras_channel", "GFP channel", n = 3, r = r, ns = ns),
        selectInput01("myhc_channel", "MyHC channel", n = 6, r = r, ns = ns),
        
        selectInput01("negative_control", "Unlabeled control", n = 1, r = r, row = TRUE, ns = ns),
        # selectInput01("positive_control_kras", "GFP-positive control", n = 2, r = r, row = TRUE, ns = ns),
        selectInput01("positive_control_myhc", "Single-dye MyHC control", n = 3, r = r, row = TRUE, ns = ns))
    })

    # create reactive expressions of the inputs above to avoid typing input$XXX every time
    # in fact, assigning only to r$XXX is sufficient, but then need to change this everywhere e.g. fsc() was used to r$fsc()

    r$fsc <- reactive(input$forward_scatter)
    r$ssc <- reactive(input$side_scatter)
    r$ch_kras <- reactive(input$kras_channel)
    r$ch_myhc <- reactive(input$myhc_channel)
    r$ctrl_kras <- reactive(input$positive_control_kras)
    r$ctrl_myhc <- reactive(input$positive_control_myhc)
    r$ctrl_negative <- reactive(input$negative_control)
    
    input_list <- reactive({
      list(input$forward_scatter, input$side_scatter, input$kras_channel, input$myhc_channel, input$negative_control, input$positive_control_kras, input$positive_control_myhc)
    })
    
    # alterts if non-unique inputs
    # (1) initially, return NULL if any of the inputs are NULL (inputs inexistant)
    # (2) then check for duplications, anyDuplicated() returns :
    #     - index i of first duplicated entry x[i] if there are any
    #     -  0 otherwise --> > 0 is therefore a good condition.

    observe({
      # input_list <- list(fsc(), ssc(), ch_kras(), ch_myhc(),
      #                    ctrl_negative(),ctrl_kras(), ctrl_myhc())
      if (any(sapply(input_list(), is.null))) return(NULL)
      else if (anyDuplicated(input_list()) > 0) {
        showModal(modalDialog("Inputs have to be unique.", title = "Warning.",
          footer = modalButton("Dismiss")))
      }
    })

    # if user wants to reset curation (input$ok), create new gs from initial fs
    observe({
      r$gs <- GatingSet(r$fs)
    }) |> bindEvent(input$ok)

    # SSC vs FSC plot of control samples --------------------------------------
    # We need a set of reactive expressions that capture the input from our selectInput widgets defined above. Since we do not want the app to perform computations every time the input changes - but rather when the user is "finished" defining his inputs - put these reactive expressions under the control of a new button "Curate" (accessed by input$Curate). Code depending on e.g. control_indices() should therefore also not update unless "Curate" is clicked (results are cached, and before input$Curate, this code would not know that the input has changed).

    # Question: with the code below, you create a reactive expression with a dependency on input$Curate. BUT, when input$Curate changes (e.g the user clicks on the button), the entire code is computed (correct?), if the results have changed or not? Here this is not a problem because accessing the inputs is not computationally expensive, but we should keep this in mind. isolate() is what you need here!

    # We now need to define a reactive expression generating our first gate. How this gate is generated obviously depends on the name of our side_scatter and forward_scatter reactive expressions (i.e which channels the user wants to gate on). These reactive expressions have a dependency on input$Curate, so they won't change unless the user clicks "Curate".

    # Question 1: The code below create a reactive expression that creates the first gate. I don't know why this does not give an error of the type "can't find function side_scatter()", because side_scatter does not exist before the user clicks "Curate".

    pgn_cut <- matrix(c(12500, 99000, 99000,0,0,6250, 6250, 99000, 99000,12500),
                      ncol = 2,
                      nrow = 5)
    
    gate_non_debris <- reactive({
      req(r$gs)
      colnames(pgn_cut) <- c(input$side_scatter, input$forward_scatter)
      polygonGate(filterId = "NonDebris", .gate = pgn_cut)
    })
    
    # add the gate to the gatingSet
    observe({
      gs_pop_add(r$gs, gate_non_debris(), parent = "root")
      message("Added the non_debris gate to the gatingSet")
      recompute(r$gs)
      message("Recomputed the gatingSet")
      }) |> bindEvent(input$Curate, ignoreInit = TRUE)

    # output plot of the nonDebris gate: we still need a bindEvent, because if not the default channel names are taken, which is not always right
    # if a user different from us uses it
    output$non_debris_gate <- renderPlot({
      ggcyto(r$gs[[c(input$negative_control, input$positive_control_myhc)]],
             aes(x = .data[[input$side_scatter]] , y = .data[[input$forward_scatter]]),
             subset = "root") +
        geom_hex(bins = 150) +
        theme_bw() +
        geom_gate(gate_non_debris()) +
        geom_stats()
    }, res = 120) |> bindEvent(input$Curate)

    # Curate background noise: KRas channel -----------------------------------
    # get_lowerLimit is a custom function: see end of document for details
    lower_limit_gfp_gate <- reactive({
      get_lowerLimit(gs = r$gs, datasets = c(input$negative_control, input$positive_control_myhc), node = "NonDebris", ch_gate = input$kras_channel, r = r)
    })
    
    ## this we don't need anymore, as we want to INCLUDE the MyHC- population (meeting BP/RC/DA: 15.05)
    # lower_limit_myhc_gate <- reactive({
    #   get_lowerLimit(gs = r$gs, datasets = c(input$negative_control, input$positive_control_kras), node = "NonDebris", ch_gate = input$myhc_channel, r = r)
    # })

    # create the final gfp gate
    # this will only be called when gfp_gate() is called, so the computation is only performed when necessary (not before input$Curate clicked)
    gfp_gate <- reactive({
      make_gate(lower_limit_gfp_gate(), input$kras_channel, filterId = "GFP+")
    })
    
    ## this we don't need anymore (meeting 15.05)
    # myhc_gate <- reactive({
    #   make_gate(lower_limit_myhc_gate(), input$myhc_channel,filterId = "MyHC+")
    # })

      observe({
        # add gate to the gatingSet: parent should be NonDebris
        gs_pop_add(r$gs, gfp_gate(), parent = "NonDebris")
        message("Added gfp_gate to the gatingSet")
        # gs_pop_add(r$gs, myhc_gate(), parent = "GFP+")
        # message("Added myhc_gate to the gatingSet")
        # recompute the GatingSet
        recompute(r$gs)
        # For testing/debugging
        message("Recomputed the gatingSet")
      }) |> bindEvent(input$Curate)

      # plot the gate
      # ATTENTION: is it in theory possible that this plot is evaluated BEFORE the observer in line 177? because below, the subset "NonDebris" only exists after this observer is executed --> potential crashing source!!!
      output$gfp_gate <- renderPlot({
        ggcyto((r$gs[[c(input$negative_control, input$positive_control_myhc)]]),
               aes(x = .data[[input$kras_channel]]),
               subset = "NonDebris") +
          geom_histogram(bins = 50, fill = "palegreen1", color = "black") +
          theme_bw() +
          geom_gate(gfp_gate()) +
          scale_x_flowjo_biexp()
      }, res = 120) |> bindEvent(input$Curate)
      
      output$gfp_gate_numeric <- renderText(paste0("The GFP-cutoff separating your GFP- and GFP+ population is set to ", round(lower_limit_gfp_gate(), digits = 1), ". Thus, cells with GFP intensities below this value are excluded from your analysis as they most likely correspond to untransfected, autofluorescent cells."))
      
      ## this we don't need anymore: meeting 15.05
      # output$myhc_gate <- renderPlot({
      #   ggcyto(isolate(r$gs[[c(input$negative_control, input$positive_control_kras)]]),
      #          aes(x = .data[[input$myhc_channel]]),
      #          subset = "NonDebris") +
      #     geom_histogram(bins = 50, fill = "pink", color = "black") +
      #     theme_bw() +
      #     geom_gate(myhc_gate()) +
      #     scale_x_flowjo_biexp()
      # }, res = 120) |> bindEvent(input$Curate)
    
      observe({
        r$lower_limit_gfp <- lower_limit_gfp_gate() 
      })|> bindEvent(input$Curate)

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
