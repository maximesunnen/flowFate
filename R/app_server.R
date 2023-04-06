#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	# Your application server logic
	r <- reactiveValues()
	mod_import_server(id = "import_1", r = r)
	mod_curate_server(id = "curate_1", r = r)
	mod_gate_server(id = "gate_1", r = r)
}
