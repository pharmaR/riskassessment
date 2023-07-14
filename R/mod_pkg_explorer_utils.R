# based on makeNodes() function in jsTreeR/inst/examples/folder/global.R
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
    label <- tail(strsplit(item, "/")[[1]], 1)
    if (length(children)) {
      list(type = "root", text = label, data = item,
           children = lapply(children, f))
    } else {
      list(type = "child", text = label, data = item)
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}