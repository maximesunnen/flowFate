testServer(
  mod_import_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    file.name <- system.file("extdata", "0877408774.B08",
                             package = "flowCore")
    expect_true(n_datasets(file.name) == 1L)
    funtreated <- system.file("demo_data_FCS3.0", "02_G12C_Untreated.fcs",
                              package = "flowFate")
    expect_true(n_datasets(funtreated) == 1L)
    # test reading one FCS file
    fs_untreated <- read.flowSet(files = funtreated, emptyValue = FALSE,
                                 truncate_max_range = FALSE, alter.names = TRUE, transformation = FALSE)
    expect_true(pData(fs_untreated)[["name"]] == "02_G12C_Untreated.fcs")
    expect_true(sampleNames(fs_untreated) == "02_G12C_Untreated.fcs")
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))

})

test_that("module ui works", {
  ui <- mod_import_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_import_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

