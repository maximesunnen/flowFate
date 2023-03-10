#' curate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggcyto
#' @import ggplot2
#' @import magrittr

mod_curate_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      uiOutput(ns("Curation_header")),
      textOutput(ns("cur_ds")),
      plotOutput(ns("debris_plot")),
      textOutput(ns("test"))
    ))
}

#' curate Server Functions
#'
#' @noRd
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

        observe({
          output$cur_ds <- renderText({
          glue::glue("Cur contains {r$nb_ds} datasets.")
            })
          withProgress(min = 1, max = 10, expr = {
            setProgress(message = 'Plotting in progress',
                        detail = 'Plotting your data...',
                        value = 3)

          setProgress(message = 'Plotting in progress',
                      detail = 'Displaying your data...',
                      value = 10)

          output$Curation_header <- renderUI({h2("Curation")})
          
          
          })}) %>% bindEvent(r$Submit, ignoreInit = TRUE)
        
        output$debris_plot <- renderPlot({
          if (length(r$s()) > 0) {
            ggcyto(r$gs[[r$s()]], aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
              geom_hex(bins = 150) +
              theme_bw()
          }
        })
        
        output$test <- renderText({glue::glue("You selected dataset number {r$s()}")})
        })
  }

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
