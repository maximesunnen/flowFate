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
             sidebarPanel(),
             mainPanel(h1("How export works.")
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
 
  })
}
    
## To be copied in the UI
# mod_export_ui("export_1")
    
## To be copied in the server
# mod_export_server("export_1")
