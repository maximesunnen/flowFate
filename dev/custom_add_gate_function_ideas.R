add_gate <- function(gs, gate, parent) {
  gate_names <- names(gate)
  gatingSet_names <- sampleNames(gs)
  
for (i in seq_along(gatingSet_names)) {
  if (gatingSet_names[i] %in% gate_names) {
    x <- which(gatingSet_names[i] == gate_names) # gives the index of the name in gate_names that matches gatingSet_names
    gs_pop_add(gs[[i]], gate = gate[[x]], parent = parent)
    print(gs[[i]])
    message("printed gs")
    print(gate[[x]])
    message("printed gate")
  }
}
  recompute(gs)
}

# #get list names: in this case the names associated with the gates
# gate_names <- names(gfp_low_myo_high)
# 
# #get names of the gating set samples
# gatingSet_names <- sampleNames(gs)

# gs_pop_add() only works if gate_names == gatingSet_names, so we need some custom function to add the gates


add_gate(gs = gs, gate = gfp_low_myo_high, parent = "MYO+")
