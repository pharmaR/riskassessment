get_exported_functions <- function(pkgdir) {
  s <- readLines(file.path(pkgdir, "NAMESPACE"))
  sexp <- s[grepl("export", s)]
  sexp <- gsub("export\\((\\w+)\\)", "\\1", sexp)
  simp <- s[grepl("importFrom", s)]
  simp <- gsub(".+,\\s*(\\w+)\\)", "\\1", simp)
  sort(setdiff(sexp, simp))
}

get_parse_data <- function(type = c("test", "source"), pkgdir, funcnames = NULL) {
  type <- match.arg(type)
  dirpath <- switch (type,
    test = file.path(pkgdir, "tests", "testthat"),
    source = file.path(pkgdir, "R")
  )
  filenames <- list.files(dirpath, ".+\\.[R|r]$")
  bind_rows(lapply(filenames, function(filename) {
    d <- parse(file.path(dirpath, filename)) %>% 
      utils::getParseData() %>% 
      dplyr::filter(token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL"))
    d <- d %>% 
      dplyr::mutate(
        type = type,
        file = filename,
        func = text,
        line = line1
      ) %>% 
      dplyr::select(type, file, func, line, token) %>% 
      dplyr::distinct()
    if (!is.null(funcnames))
      d <- d %>% dplyr::filter(func %in% funcnames)
    d
  }))
}

get_test_files <- function(funcname, parse_data) {
  parse_data %>%
    dplyr::filter(type == "test",
                  func == funcname) %>% 
    dplyr::pull(file) %>% 
    unique()
}

get_source_files <- function(funcname, parse_data) {
  parse_data %>%
    dplyr::filter(type == "source", 
                  func == funcname) %>% 
    dplyr::pull(file) %>% 
    unique()
}

get_man_files <- function(funcname, pkgdir) {
  man_files <- list.files(file.path(pkgdir, "man"), ".+\\.Rd$")
  i <- sapply(man_files, function(f) {
    s <- readLines(file.path(pkgdir, "man", f))
    any(grepl(sprintf("name\\{%s\\}|alias\\{%s\\}", funcname, funcname), s))
  })
  man_files[i]
}

get_files <- function(funcname, type = c("test", "source", "man"), ...) {
  type <- match.arg(type)
  switch(
    type,
    test = get_test_files(funcname, ...),
    source = get_source_files(funcname, ...),
    man = get_man_files(funcname, ...)
  )
}