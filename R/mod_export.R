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

           # Defining the sidebarLayout in the Export tab ------------------------------
           sidebarLayout(
             # Defining the sidebarPanel
             sidebarPanel(
               textInput(ns("filename"), label = "Filename", placeholder = "Insert your filename here"),
               checkboxInput(ns("add_date"), label = "Add system date to filename"),
               downloadButton(ns("download"), "Download", class = "btn-primary"),
               actionButton(ns("table"), label = "Show population statistics", class = "btn-primary")),

             # Defining the mainPanel
             mainPanel(
               tabsetPanel(id = ns("tabset"),
                           tabPanel("Information", icon = icon("info"),
                                    h1(strong("How export works.")),
                                    br(),
                                    div(
                                      p("Click ", span("Show population statistics", style = "color:#008cba; font-weight:bold"), " to generate the population statistics table."),
                                      p("Export this table by entering a filename in the sidebar menu. Tick the small checkbox below the filename to append the system date (i.e today's data if set correctly on your PC) to your file name."),
                                      p(strong("Note:"), "The file extension defaults to .csv. Don't add a file extension to your filename."),

                                      p("Click on the ", span("Download", style = "color:#008cba; font-weight:bold"), "button to download this table as a csv file."), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
                                    ), br(),
                                    div(
                                      p("The population statistics table has 3 columns:"),
                                      p("-  ", strong("sample:"), " dataset (defaults to 'dataset' + well number)", style = "text-indent: 25px"),
                                      p("-  ", strong("pop:"), " the population (for example /NonDebris/GFP+ comprises cells that are not debris and GFP-positive", style = "text-indent: 25px"),
                                      p("-  ", strong("count"), ": population size (cell count)", style = "text-indent: 25px"), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"), br()),
                           tabPanel("Population statistics",
                                    br(),
                                    tableOutput(ns("population_table"))
                           )))))}

#' export Server Functions
#'
#' @noRd
#' @importFrom utils write.table
mod_export_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Making a reactive expression file_name() corresponding to the file name entered by the user in the input$filename. If the user does not enter a       file name, a default file name "population statistics.csv" is used
    ### user has the option to add the system date by checking the checkbox input$add_date
    file_name <- reactive({
      if (isTruthy(input$filename)) {
        if (input$add_date == TRUE) {paste0(Sys.Date(), "_", input$filename,".csv")}
        else {paste0(input$filename, ".csv", sep = "")}
      }
      else {
        if (input$add_date == TRUE) {paste0(Sys.Date(), "_", "population_statistics.csv")}
        else {paste0("population_statistics.csv")}
      }
    })

    # Making a reactive expression population table that computes the final population statistics table users need after their analysis
    ### old version : population_table <- reactive({purrr::map_df(r$gs, \(x) as.data.frame(gs_pop_get_count_fast(x)))}), but we want the table obtained     by gs_pop_get_stats())
    ### to each element of r$gs, the function as.data.frame(gs_pop_get_stats()) is applied. map_df returns a df.

    population_table <- reactive({
      purrr::map_df(r$gs, \(x) as.data.frame(gs_pop_get_stats(x)))
    })

    # Defining the output$population_table. Bind this output to input$table to not constantly recompute this table during previous analysis steps
    output$population_table <- renderTable({population_table()}) |> bindEvent(input$table)

    # Defining the csv comments
    text <- reactive({
      if (!is.null(r$low_bin) && is.null(r$medium_bin)) {
        c(paste0("#flowFate-Version: ", packageVersion("flowFate")),
          paste0("#Red channel: ", r$ch_myhc(),";"),
          paste0("#Green channel: ", r$ch_kras(),";"),
          paste0("#low bin: ", r$low_bin,";"))
      }
      else if (!is.null(r$medium_bin) && is.null(r$high_bin)) {
        c(paste0("#flowFate-Version: ", packageVersion("flowFate")),
          paste0("#Red channel: ", r$ch_myhc(),";"),
          paste0("#Green channel: ", r$ch_kras(),";"),
          paste0("#low bin: ", r$low_bin,";"),
          paste0("#medium bin: ", r$medium_bin,";"))
      }
      else if (!is.null(r$high_bin)) {
        c(paste0("#flowFate-Version: ", packageVersion("flowFate")),
          paste0("#Red channel: ", r$ch_myhc(),";"),
          paste0("#Green channel: ", r$ch_kras(),";"),
          paste0("#low bin: ", r$low_bin,";"),
          paste0("#medium bin: ", r$medium_bin,";"),
          paste0("#high bin: ", r$high_bin,";"))
      }
    })

    # Defining the output$download download handler that allows the user to download the table as a csv file
    output$download <- downloadHandler(filename = function() file_name(),
                                       content = function(file){
                                         writeLines(file,
                                                    text = text())
                                         write.table(population_table(), file = file, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)})

    # Observer to update the tabset from "Information" to "Population statistics" when the user clicks input$table
    observe({
      updateTabsetPanel(inputId = "tabset", selected = "Population statistics")
    }) |> bindEvent(input$table)

  })}

## To be copied in the UI
# mod_export_ui("export_1")

## To be copied in the server
# mod_export_server("export_1")
