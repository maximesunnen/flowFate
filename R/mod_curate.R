#' curate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_curate_ui <- function(id){
  ns <- NS(id)

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

               textOutput(ns("cur_ds")),
               textOutput(ns("test"))
             )))}

#' curate Server Functions
#'
#' @noRd
#' @importFrom purrr is_null
#' @import ggplot2
#' @importFrom ggcyto ggcyto geom_gate
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # All selections from sidebar (control datasets and channels) -------------
    output$channel_selection <- renderUI({
      req(r$gs)

      tagList(
      selectInput(ns("fsc_channel"),
                  "Forward Scatter",
                  choices = c("", colnames(r$gs)),
                  selected = colnames(r$gs)[1]),
      
      selectInput(ns("ssc_channel"),
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
      
      selectInput(ns("kras_control"),
                  "Positive control (KRAS)",
                  choices = c("", rownames(pData(r$fs))),
                  selected = rownames(pData(r$fs))[2]),

      selectInput(ns("myhc_control"),
                  "Positive control (MYHC)",
                  choices = c("", rownames(pData(r$fs))),
                  selected = rownames(pData(r$fs))[3])
      )
    })

    # SSC vs FSC plot of control samples --------------------------------------
    ## get indices (is it really indices???) of the datasets selected
    control_indices <- reactive(c(input$kras_control,
                                  input$myhc_control, 
                                  input$negative_control))
    
    ssc <- reactive(input$ssc_channel)
    fsc <- reactive(input$fsc_channel)


observe({
  gate <- exclude_debris()
  output$non_debris_gate <- renderPlot({
    ggcyto(r$gs[[control_indices()]],
           aes(x = .data[[ssc()]] , y = .data[[fsc()]]),
           subset = "root") +
      geom_hex(bins = 150) +
      theme_bw() +
      geom_gate(gate) +
      geom_stats()
  })
}) %>% bindEvent(input$Curate, ignoreInit = TRUE)
  })
}



# here we should also be able to provide an input$ssc to not explicitly name "SSC.HLin" because these might be called differently for another user



exclude_debris <- reactive({
  pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),
                    ncol = 2,
                    nrow = 5)
  colnames(pgn_cut) <- c("SSC.HLin", "FSC.HLin")
  polygonGate(filterId = "NonDebris", .gate = pgn_cut)
})

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
