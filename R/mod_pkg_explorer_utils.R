# based on makeNodes() function in jsTreeR/inst/examples/folder/global.R
#' @importFrom utils tail
#' @noRd
makeNodes <- function(leaves) {
  dfs <- lapply(strsplit(leaves, "/"), function(s) {
    item <- Reduce(function(a, b) paste0(a, "/", b), s[-1], s[1], accumulate = TRUE)
    data.frame(item = item, parent = c("root", item[-length(item)]))
  })
  dat <- dfs[[1]]
  for(i in 2:length(dfs)){
    dat <- merge(dat, dfs[[i]], all = TRUE)
  }
  f <- function(parent){
    i <- match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent == item]
    label <- utils::tail(strsplit(item, "/")[[1]], 1)
    if (length(children)) {
      list(type = "root", text = label, data = item,
           children = lapply(children, f))
    } else {
      list(type = "child", text = label, data = item)
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}

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
