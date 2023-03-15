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
               
               plotOutput(ns("gfp_gate"))
             )))}

#' curate Server Functions
#'
#' @noRd
#' @importFrom purrr is_null
#' @import ggplot2
#' @importFrom ggcyto ggcyto geom_gate geom_stats
#' @import flowWorkspace


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
    
    #compute all these reactive expressions only when input$Curate is activated: so only dependency on input$Curate, not on the changing of the selectInput. This also means that code depending on e.g. control_indices() will not update unless you click on curate [results are cached, and before input$Curate, this code would not know that the input has changed]
    ## question: is my understanding here correct? I think that with eventReactive you create a reactive dependency on the first argument (here input$Curate), so when this updates, the entire code is computed, regardless of the fact that input$kras_control etc changed or not?
    
    control_indices <- eventReactive(input$Curate, c(input$kras_control, 
                                                     input$myhc_control, 
                                                     input$negative_control))
    
    ssc <- eventReactive(input$Curate, {input$ssc_channel})
    fsc <- eventReactive(input$Curate, {input$fsc_channel})
    

# define the polygon gate matrix ------------------------------------------
  # can maybe be placed outside the server function
    
    pgn_cut <- matrix(c(0, 12500, 99000, 99000,0,6250, 6250, 6250, 99000, 99000),
                      ncol = 2,
                      nrow = 5)
    
    # define a reactive expression which generates the gate, since it depends on ssc() and fsc() (which themselves depend on input$Curate), they don't trigger unless they have input.
    # However, I have no idea why this is not giving a sort of "don't know how to deal with object of type NULL" error....)
    # -->  now i know why: ssc() has an initial value, since i added "selected =... " argument to selectInput, so initially ssc() does exist and is not NULL!!! BUT only after "Curate"???)
    # ideally, pgn-cut should  be out of the reactive, since it does not depend on any reactive input(and it does not have to be recomputed every time ssc() changes!
    #  polygonGate however HAS to be inside, because it uses ssc() and fsc()
    
    #very important question: when ssc() changes (because selectInput changed and input$Curate was activated, is polygonGate updated (i think yes)? or does pgn_cut have to be a reactive for this and used in polygonGate as .gate = pgn_cut()) (i think no)
    #exlude debris should not be in observe({}) since you can't use observers in other statements, they're made for their side effects!

    exclude_debris <- reactive({
      colnames(pgn_cut) <- c(ssc(), fsc())
      polygonGate(filterId = "NonDebris", .gate = pgn_cut)
    })

    
  #control_incides() is under the control of input$Curate. It's funny that i don't get an error of the type: "Can't subset r$gs" because initially control_indices() does not exist. does this have to do with lazyness? render*_ functions only compute their content when it's necessary? when drawing the reactive graph it makes sense! ssc() cannot be computed, therefore exclude_debris() cannot be computed, and ultimately the renderPlot({}) is stuck at the first line and will not try to compute ggcyto (which would lead to an error?)

    ## is this good practice here to put other stuff than the actual plot inside renderPlot()?
    
  # output$non_debris_gate <- renderPlot({
  #   # gate_non_debris <- 
  #   #exclude_debris()
  #   message("okay")
  #   # add gate to gs
  #   gs_pop_add(r$gs, exclude_debris(), parent = "root")
  #   # recompute the GatingSet
  #   message("Added the non_debris gate to the gatingSet")
  #   recompute(r$gs)
  #   message("Recomputed the gatingSet")
  #   ggcyto(r$gs[[control_indices()]],
  #          aes(x = .data[[ssc()]] , y = .data[[fsc()]]),
  #          subset = "root") +
  #     geom_hex(bins = 150) +
  #     theme_bw() 
  #     geom_gate(exclude_debris())
  #     geom_stats()
  # })
  # 

observe({
  colnames(pgn_cut) <- c(ssc(), fsc())
  gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
  message("created the gate")
  # add gate to gs
  gs_pop_add(r$gs, exclude_debris(), parent = "root")
  # recompute the GatingSet
  message("Added the non_debris gate to the gatingSet")
  recompute(r$gs)
  message("Recomputed the gatingSet")
  output$non_debris_gate <- renderPlot({
    ggcyto(r$gs[[control_indices()]],
         aes(x = .data[[ssc()]] , y = .data[[fsc()]]),
         subset = "root") +
    geom_hex(bins = 150) +
    theme_bw()+
    geom_gate(gate_non_debris)+
    geom_stats()
})}) |> bindEvent(input$Curate, ignoreInit = TRUE)


  })
}

# )}

# here we should also be able to provide an input$ssc or ssc() to not explicitly name "SSC.HLin" because these might be called differently for another user. somehow this is not working: if I add c(ssc(), fsc()) it says "error in ssc: could not find function "ssc""




## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
