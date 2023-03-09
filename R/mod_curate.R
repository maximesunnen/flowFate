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
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h2("Curation"),
      textOutput(ns("cur_ds"))
    ))
}

#' curate Server Functions
#'
#' @noRd
mod_curate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$cur_ds <- renderText({
      glue::glue("Cur ontains {nb_ds()} datasets.")})
  })
}

## To be copied in the UI
# mod_curate_ui("curate_1")

## To be copied in the server
# mod_curate_server("curate_1")
