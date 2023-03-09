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

mod_curate_ui <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      uiOutput(ns("Curation_header")),
      textOutput(ns("cur_ds")),
      plotOutput(ns("debris_plot"))
    ))
}

#' curate Server Functions
#'
#' @noRd
mod_curate_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(r$Submit, {

      output$cur_ds <- renderText({
      glue::glue("Cur contains {r$nb_ds()} datasets.")
        })

      output$debris_plot <- renderPlot({
        ggcyto(r$gs, aes(x = SSC.HLin, y = FSC.HLin), subset = "root") +
          geom_hex(bins = 150) +
          theme_bw()
        })

      output$Curation_header <- renderUI({h2("Curation")})
    })
  })
}

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
