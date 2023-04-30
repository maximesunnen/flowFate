#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_explore_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title = "Explore",
           # Defining a sidebarlayout in the import tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               tagList(
                 selectInput(ns("plot.type"), label = "Select plot type", choices = c("","scatter", "density", "histogram")),
                 uiOutput(ns("x.channel")),
                 uiOutput(ns("y.channel")))),
             
             mainPanel(
               plotOutput(ns("explore.plot"))
)))
}
    
#' explore Server Functions
#'
#' @noRd 
mod_explore_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
output$x.channel <- renderUI({
  selectInput(ns("x.axis"), label = "Select X axis", choices = c("",colnames(r$gs)))
})
output$y.channel <- renderUI({
  selectInput(ns("y.axis"), label = "Select Y axis", choices = c("",colnames(r$gs)))
})

x_channel <- reactive(input$x.axis)
y_channel <- reactive(input$y.axis)

 output$explore.plot <- renderPlot({
   if(input$plot.type == "scatter") {
     req(input$x.axis, input$y.axis)
     ggcyto(data = r$gs, aes(x = .data[[x_channel()]], y = .data[[y_channel()]]), subset = "root")+
       geom_hex(bins = 200)+
       # scale_x_flowjo_biexp() +
       theme_bw()
   }
   else if (input$plot.type == "histogram"){
     req(input$x.axis)
     # why exactly do we need this .data[[...]]? can't remember
     ggcyto(data = r$gs, aes(x = .data[[x_channel()]]), subset = "root")+
       geom_histogram()+
       scale_x_flowjo_biexp() +
       theme_bw()
   }
   else if (input$plot.type == "density") {
     req(input$x.axis)
     ggcyto(data = r$gs, aes(x = .data[[x_channel()]]), subset = "root")+
       geom_density()+
       scale_x_flowjo_biexp() +
       theme_bw()
   }

 })
  })
}
    
## To be copied in the UI
# mod_explore_ui("explore_1")
    
## To be copied in the server
# mod_explore_server("explore_1")
