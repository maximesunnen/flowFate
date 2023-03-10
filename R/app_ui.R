#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # navbarPage(title = "FlowFate",
    #            tabPanel(title = "Import", 
    #                     mod_import_ui("import_1")),
    #            tabPanel(title = "Curate",
    #                     mod_curate_ui("curate_1")),
    #                     inverse = TRUE)
    fluidPage(theme = bslib::bs_theme(bootswatch = "united"),
      navbarPage(title = "FlowFate",
      mod_import_ui("import_1"),
      mod_curate_ui("curate_1")
    )
    )
    )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "flowFate"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
