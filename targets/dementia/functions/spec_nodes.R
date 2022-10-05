



spec_nodes <- function(baseline_vars, longitudinal_vars, num_time){
  node_names <- c(baseline_vars, expand.grid(longitudinal_vars, num_time) %>%
                    apply(1, function(row) paste0(row, collapse = "")))
  node_names <- gsub(" ","",node_names)
  return(node_names)
}
