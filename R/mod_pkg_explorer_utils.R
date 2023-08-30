#' Makes shinyTree Nodes
#' 
#' Creates nodes in the structure anticipated by shinyTree
#' 
#' @noRd
#' 
#' @importFrom purrr reduce
make_nodes <- function(leaves) {
  f <- function(parent) {
    if (length(parent) == 1) return(setNames(list(structure("",sttype="file")), parent))
    root_dir <- parent[1]
    sub_dir <- parent[-1]
    setNames(list(f(sub_dir)), root_dir)
  }
  merge_lists <- function(base, overlay) {
    if (length(base) == 0)
      return(overlay)
    if (length(overlay) == 0)
      return(base)
    keys <- unique(c(names(base), names(overlay)))
    out_list <- list()
    for (key in keys) {
      out_list[[key]] <- merge_lists(base[[key]], overlay[[key]])
    }
    out_list
  }
  lapply(strsplit(leaves, "/"), f) %>%
    purrr::reduce(merge_lists)
}

#' Get List Element
#' 
#' Helper function to get list element based on selected tree leaf
#' 
#' @noRd
get_list_element <- function(selected, list) {
  if (is.null(list)) return(NULL)
  if (!is.list(selected)) return(list)
  key <- names(selected)[1]
  get_list_element(selected[[key]], list[[key]])
}

#' Get Selected Path
#' 
#' Helper function to get the filepath based on the selected tree leaf
#' 
#' @noRd
get_selected_path <- function(selected) {
  if (!is.list(selected)) return(NULL)
  key <- names(selected)[1]
  paste(c(key, get_selected_path(selected[[key]])), collapse = "/")
}
