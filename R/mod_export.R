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
               textInput(ns("filename"), label = "Filename", placeholder = "Insert your filename here"),
               checkboxInput(ns("add_date"), label = "Add system date to filename"),
               downloadButton(ns("download"), "Download", class = "btn-primary"),
               actionButton(ns("table"), label = "Show population statistics", class = "btn-primary")),
             mainPanel(
               h1(strong("How export works.")),
               br(),
               div(
                 p("First, click on the ", span("Show population statistics", style = "color:#008cba; font-weight:bold"), " button to display a table containing statistics on the different populations seperated during this workflow."),
                 p("To export this table, enter your filename on the left. By ticking the small checkbox below the entry field, the system date (today's data, if  set correctly on your computer) will be added automatically to your file name."),
                 p(strong("Note:"), "The file extension will automatically be set to .csv, so you don't have to (and should not) add it manually."),

                 p("Click on the ", span("Download", style = "color:#008cba; font-weight:bold"), "button to download this table as a csv file."), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"
               ),
               br(),
               div(
                 p("The exported population statistics table contains 5 columns:"),
                 p("-  ", strong("name"), ": indicates the dataset and defaults to \"dataset_well-number\"", style = "text-indent: 25px"),
                 p("-  ", strong("Population"), ": indicates the population created after applying a gate i.e /NonDebris contains all the events that are not debris, /NonDebris/GFP+ contains all the events that are not debris and are GFP-positive, etc. The population and gate names are identical.", style = "text-indent: 25px"),
                 p("-  ", strong("Count"), ": indicates the number of events in the respective population", style = "text-indent: 25px"),
                 p("-  ", strong("ParentCount"), ": indicates the number of events in a parent population i.e. for the Population /NonDebris/GFP+, the ParentCount corresponds to the number of events in the NonDebris population.", style = "text-indent: 25px"),
                 p("-  ", strong("Percentage"), ": (Count/ParentCount) x 100", style = "text-indent: 25px"), style = "text-align:justify;color:ck;background-color:#f8f8f8;padding:15px;border-radius:10px"),
               br(),
               textOutput(ns("test")),
               tableOutput(ns("population_table")))))}
    
#' export Server Functions
#'
#' @noRd 
#' @importFrom utils write.csv
#' @importFrom dplyr mutate lag
mod_export_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    file_name <- reactive({
      if (isTruthy(input$filename)) {
        if (input$add_date == TRUE) {
          paste0(Sys.Date(), "-", input$filename,".csv")
        }
        else {
          paste0(input$filename, ".csv", sep = "")
        }
      }
        else {
          if (input$add_date == TRUE) {
          paste0(Sys.Date(), "-", "population_statistics.csv")
          }
          else {
            paste0("population_statistics.csv")
          }
          }
    })

    
    output$test <- reactive(file_name())
    
    output$population_table <- renderTable({population_table()}) |> bindEvent(input$table)
    
    output$download <- downloadHandler(filename = function() file_name(), content = function(file) {write.csv(population_table(), file)})

    # reactive expression computing the final table we want to obtain
    # old version : population_table <- reactive({ purrr::map_df(r$gs, \(x) as.data.frame(gs_pop_get_count_fast(x)))})

    population_table <- reactive({
      purrr::map_df(r$gs, \(x) as.data.frame(gs_pop_get_stats(x)) |>  mutate("Percentage" = ifelse(is.na(count / lag(count)), (count / count) * 100, (count / lag(count)) * 100)))
    })
      }
    )
  }

## To be copied in the UI
# mod_export_ui("export_1")
    
## To be copied in the server
# mod_export_server("export_1")
