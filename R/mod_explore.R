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
                 radioButtons(ns("scale"), label = NULL, choices = c("linear", "biexponential")),
                 uiOutput(ns("x.channel")),
                 uiOutput(ns("y.channel")),
                 DTOutput(ns("individual_FCS"))
                 # uiOutput(ns("datasets"))
                 )),
             
             mainPanel(
               fluidRow(
               column(4,
                      textInput(ns("download.filename"), label = NULL, placeholder = "Enter filename")),
               column(3,
                      downloadButton(ns("download"), label = "Download .svg")),
               column(3, 
                      downloadButton(ns("download.all"), label = "Download all .svgs"))),
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

# output$datasets <- renderUI({
#   selectInput(ns("dataset"), label = "Select dataset", choices = c("",sampleNames(r$gs)))
# })

# x_channel <- reactive(input$x.axis)
# y_channel <- reactive(input$y.axis)

# dataset <- reactive(input$dataset)

selected_rows <- reactive(input$individual_FCS_rows_selected)

output$explore.plot <- renderPlot({explore_plot()}, res = 120)

#' @importFrom ggcyto scale_y_flowjo_biexp scale_x_flowjo_biexp

explore_plot <- reactive({
   req(selected_rows())
   if(input$plot.type == "scatter") {
     req(input$x.axis, input$y.axis)
     if (input$scale == "biexponential"){
       ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]], y = .data[[input$y.axis]]), subset = "root")+
         geom_hex(bins = 200)+
         scale_x_flowjo_biexp() +
         scale_y_flowjo_biexp() +
         theme_bw()
     }
     else {
       ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]], y = .data[[input$y.axis]]), subset = "root")+
       geom_hex(bins = 200)+
       # scale_x_flowjo_biexp() +
       theme_bw()
     }
   }
   else if (input$plot.type == "histogram"){
     req(input$x.axis)
     # why exactly do we need this .data[[...]]? can't remember
     ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root")+
       geom_histogram()+
       scale_x_flowjo_biexp() +
       theme_bw()
   }
   else if (input$plot.type == "density") {
     req(input$x.axis)
     ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root")+
       geom_density()+
       scale_x_flowjo_biexp() +
       theme_bw()
   }
 })
 
 output$individual_FCS <- renderDT({r$flowSet_pData()}, selection = list(target = "row", selected = 1, mode = "single"), rownames = FALSE, class = "cell-border stripe", options = list(paging = FALSE, scrollY = "200px"))
 
 download_filename <-reactive({
   if (isTruthy(input$download.filename)) {
     paste0(input$download.filename, ".svg", sep = "")
   }
 })
 
 output$download <- downloadHandler(filename = function() download_filename(), content = function(file) {ggsave(file, plot = explore_plot(), device = "svg")})
  })
}
    
## To be copied in the UI
# mod_explore_ui("explore_1")
    
## To be copied in the server
# mod_explore_server("explore_1")
