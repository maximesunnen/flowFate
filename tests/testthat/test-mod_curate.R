testServer(
  mod_curate_server,
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
    # loading from the demo folder
    demo_files <- system.file("demo_data_FCS3.0",
                              package = "flowFate")
    demo_fs <- read.flowSet(path = demo_files,
                            truncate_max_range = FALSE,
                            alter.names = TRUE,
                            transformation = FALSE,
                            emptyValue = FALSE)
    expect_true(nrow(pData(demo_fs)) == 5L)
    gs <- GatingSet(demo_fs)
    r <- list(gs = gs)
    pgn_cut <- matrix(c(12500, 99000, 99000,0,0,6250, 6250, 99000, 99000,12500), ncol = 2, nrow = 5)
    colnames(pgn_cut) <- c("SSC.HLin", "FSC.HLin")
    gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
    # lower_limit_gfp_gate <- get_lowerLimit(gs = gs,
    #                                        datasets = c("00_Double-negative.fcs",
    #                                                     "02_G12C_Untreated.fcs"),
    #                node = "NonDebris", ch_gate = "RED.R.HLin", r = r)

})

test_that("module ui works", {
  ui <- mod_curate_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_curate_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

