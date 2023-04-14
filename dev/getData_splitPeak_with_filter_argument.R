
remove_null_from_list <- function(data) {
  test <- which(sapply(data, is.null))
  if (sum(test) == 0) {
    return(data)
  }
  else {
    return(data[-test])
  }
}

gfp_low_myo_high <- getData_splitPeak(gs, bin = "GFP-low", filter_name = "blabla")
  
test <- function(bla) {
    fsApply(data_gfp_low, function(fr, filterId = bla) {
      return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density", filterId = filterId), error = function(e) NULL))
    })
  }

test_function <- function(fr) {
  return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density"), error = function(e) NULL))
}

#show this to Aurelien, if filtername is replaced with filterId, code is not working

getData_splitPeak <- function(gs, bin, filter_name) {
  x <- gs_pop_get_data(gs, bin) |> cytoset_to_flowSet()
  x <- fsApply(data_gfp_low, function(fr, filterId = filter_name) {
    return(tryCatch(gate_flowclust_1d(fr,params = "RED.R.HLin",K = 2, cutpoint_method = "min_density", filterId = filterId), error = function(e) NULL))
  })
  return(remove_null_from_list(x))
}
