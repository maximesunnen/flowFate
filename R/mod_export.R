#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_export_ui <- function(id){
  ns <- NS(id)
  tabPanel(title = "Export",
           
           # Defining a sidebarLayout in the Curate tab ------------------------------
           sidebarLayout(
             sidebarPanel(
               textInput(ns("filename"), label = "Filename", placeholder = "Type your filename here"),
               downloadButton(ns("download"), "Download")
             ),
             mainPanel(
               h1("How export works."),
               textOutput(ns("test"))
             )
           )
  )
}
    
#' export Server Functions
#'
#' @noRd 
mod_export_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    file_name <- reactive({
      if (isTruthy(input$filename)) {
        if (input$filename != "Add your filename here!") {
          paste(input$filename, ".csv", sep="")
        }
        else {
          paste0("population_statistics.csv")
        }
      }
      else {
        paste0("population_statistics.csv")
      }
    })
    output_test <- cars
    output$download <- downloadHandler(filename = file_name(), content = function(file) {write.csv(output_test, file)})
    output$test <- renderText({file_name()})
      }
    )
  }
    
## To be copied in the UI
# mod_export_ui("export_1")
    
## To be copied in the server
# mod_export_server("export_1")
