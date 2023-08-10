testServer(
  mod_gate_server,
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
    gs <- GatingSet(demo_fs)
    r <- list(gs = gs)
    pgn_cut <- matrix(c(12500, 99000, 99000,0,0,6250, 6250, 99000, 99000,12500), ncol = 2, nrow = 5)
    colnames(pgn_cut) <- c("SSC.HLin", "FSC.HLin")
    gate_non_debris <- polygonGate(filterId = "NonDebris", .gate = pgn_cut)
    gs_pop_add(gs, gate_non_debris, parent = "root")
    recompute(gs)
    # now Gating, first interval 25 - 100
    test_gate <- list(low = list(c(25, 100)))
    names(test_gate[[1]]) <- "GRN.B.HLin"
    y <- rectangleGate(test_gate[[1]])
    y@filterId <- "GFP-low"
    gfp_gate <- make_gate(25, "GRN.B.HLin", filterId = "GFP+")
    nb_nodes <- gs_pop_add(gs, gfp_gate, parent = "NonDebris")
    expect_true(nb_nodes == 3L)
    expect_message(recompute(gs), "done!")
    expect_true(gs_pop_add(gs, y, parent = "GFP+") == 4L)
    expect_message(recompute(gs), "done!")
    expect_identical(gs_pop_get_count_fast(gs)$Count,
                      c(9587L, 153L, 149L, 4044L, 7L , 6L, 6080L, 2977L, 2100L, 5579L, 2944L,
                        2052L, 6703L, 1493L, 1083L))
    expect_identical(gs_pop_get_count_fast(gs)$ParentCount,
                     c(20000L, 9587L, 153L, 20000L, 4044L, 7L, 20000L, 6080L, 2977L,
                       20000L, 5579L, 2944L, 20000L, 6703L, 1493L))
    #  ; 101 350 ; 351 - 1000
})

test_that("module ui works", {
  ui <- mod_gate_ui(id = "test")
  golem::expect_shinytag(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_gate_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

