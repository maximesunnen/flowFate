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
           # Defining a sidebarlayout in the Explore tab ------------------------------
           sidebarLayout(
             # Defining a sidebarPanel
             sidebarPanel(
               tagList(
                 selectInput(ns("plot.type"), label = "Select plot type", choices = c("","scatter", "density", "histogram")),
                 radioButtons(ns("scale"), label = NULL, choices = c("linear", "biexponential")),
                 uiOutput(ns("x.channel")),
                 sliderInput(ns("bin.number"), label = "Select bin number", min = 25, max = 100, value = 50, step = 5),
                 colourInput(ns("x.channel.colour"), label = NULL, value = "grey60", closeOnClick = TRUE),
                 uiOutput(ns("y.channel")),
                 DTOutput(ns("individual_FCS")))),
             # Defining a main$Panel
             mainPanel(
               fluidRow(
                 column(4,
                        textInput(ns("download.filename"), label = NULL, placeholder = "Enter filename")),
                 column(3,
                        downloadButton(ns("download"), label = "Download .svg"))),
               plotOutput(ns("explore.plot"))
             )))
}

#' explore Server Functions
#'
#' @noRd
#' @importFrom ggcyto scale_y_flowjo_biexp scale_x_flowjo_biexp
#' @importFrom colourpicker colourInput
mod_explore_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Defining the x.channel output: a selectInput where the user can choose which channel to display on the x-axis of the plot
    output$x.channel <- renderUI({
      selectInput(ns("x.axis"), label = "Select X axis", choices = c("",colnames(r$gs)))
    })
    # Defining the y.channel output: a selectInput where the user can choose which channel to display on the y-axis of the plot
    output$y.channel <- renderUI({
      selectInput(ns("y.axis"), label = "Select Y axis", choices = c("",colnames(r$gs)))
    })

    # Defining the individual_FCS output: a table with the datasets of the uploaded file (flowSet_pData shared from import module using stratégie du petit r)
    output$individual_FCS <- renderDT({r$flowSet_pData},
                                      selection = list(target = "row", selected = 1, mode = "single"),
                                      rownames = FALSE, class = "cell-border stripe", options = list(paging = FALSE, scrollY = "200px"))

    # making a reactive expression selected_rows() corresponding to the row selected by the user
    selected_rows <- reactive(input$individual_FCS_rows_selected)

    # making a reactive expression bin_number() corresponding to the number of bins defined by the user in the sliderInput (if histogram selected as plot type)
    bin_number <- reactive(input$bin.number)

    # making a reactive expression colour_x() corresponding to the plot color selected by the user (if histogram or density selected as plot type)
    colour_x <- reactive({input$x.channel.colour})

    #making a reactive expression explore.plot.base(): this is the basic plot without a geom_* layer. We add the geom_* layer in a different plot so that when the        user changes e.g. the color of the histogram (saved in a REACTIVE expression), the entire plot is not recomputed, but only the color changes.
    #The explore.plot.base is cached and is not changing when the user changes the bin number or color.

    explore_plot.base <- reactive({
      if (input$plot.type == "scatter") {
        req(selected_rows())
        req(input$x.axis, input$y.axis)
        if (input$scale == "biexponential") {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]], y = .data[[input$y.axis]]), subset = "root") +
            scale_x_flowjo_biexp() +
            scale_y_flowjo_biexp() +
            theme_bw()
        }
        else {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]], y = .data[[input$y.axis]]), subset = "root") +
            theme_bw()
        }
      }
      else if (input$plot.type == "density") {
        req(input$x.axis)
        if (input$scale == "biexponential") {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root") +
            scale_x_flowjo_biexp() +
            theme_bw()
        }
        else {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root") +
            theme_bw()
        }
      }
      else if (input$plot.type == "histogram") {
        req(input$x.axis)
        if (input$scale == "biexponential") {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root") +
            scale_x_flowjo_biexp() +
            theme_bw()
        }
        else {
          ggcyto(data = r$gs[[selected_rows()]], aes(x = .data[[input$x.axis]]), subset = "root") +
            theme_bw()
        }
      }
    })

    # making a reactive expression explore.plot(), which now contains the geom_* layer. It calls the explore_plot.base reactive expression.
    explore_plot <- reactive({
      if (input$plot.type == "scatter") {
        explore_plot.base() + geom_hex(bins = 200)
      }
      else if (input$plot.type == "density") {
        explore_plot.base() + geom_density(fill = colour_x(), color = "black")
      }
      else if (input$plot.type == "histogram") {
        explore_plot.base() + geom_histogram(bins = bin_number(), fill = colour_x(), color = "black")
      }
    })

    # making a reactive expression download_filename(), which corresponds to either
    # - the filename entred by the user
    # - or, if no filename entered, the default filename "plot.svg"
    download_filename <- reactive({
      if (isTruthy(input$download.filename)) {
        paste0(input$download.filename, ".svg", sep = "")
      }
      else {
        paste0("plot.svg")
      }
    })

    # output plot displayed to the user. Calls the explore_plot() reactive expression
    output$explore.plot <- renderPlot({
      if (is.null(selected_rows())) return(NULL)
          else explore_plot()}, res = 120)

    # output download handler, saving the plot as .svg
    output$download <- downloadHandler(filename = function() download_filename(),
                                    content = function(file) {ggsave(file,  plot = explore_plot(), device = "svg")})

    # conditional visibility of some sidebarPanel widgets
    observe({
      if (input$plot.type == "") {
        shinyjs::hide("bin.number")
        shinyjs::hide("x.channel.colour")
      }
      else if (input$plot.type == "histogram") {
        shinyjs::hide("y.channel")
        shinyjs::show("x.channel")
        shinyjs::show("bin.number")
        shinyjs::show("x.channel.colour")
      }
      else if (input$plot.type == "density") {
        shinyjs::hide("bin.number")
        shinyjs::hide("y.channel")
        shinyjs::show("x.channel.colour")
        shinyjs::show("x.channel")
      }
      else if (input$plot.type == "scatter") {
        shinyjs::hide("bin.number")
        shinyjs::hide("x.channel.colour")
        shinyjs::show("x.channel")
        shinyjs::show("y.channel")
      }
    })
  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
