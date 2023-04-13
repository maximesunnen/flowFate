getData_splitPeak <- function(gs, bin) {
  x <- gs_pop_get_data(gs, bin) |> cytoset_to_flowSet()
  x <- fsApply(x, test_function)
  return(x)
}

test_function <- function(fr) {
  return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density"),
                  error = function(e) NULL))
}

remove_null_from_list <- function(data) {
  test <- which(sapply(data, is.null))
  if (sum(test) == 0) {
    return(data)
  }
  else {
    return(data[-test])
  }
}

a <- getData_splitPeak(gs, "GFP-medium")
b <- remove_null_from_list(a)

data_gfp_medium <- gs_pop_get_data(gs, y = "GFP-medium") |> cytoset_to_flowSet()

if (length(b) == 0) {
  b <- list()
}
