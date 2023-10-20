#' Get Exported Functions
#' 
#' Scrapes NAMESPACE for exported functions and returns them as a list
#' 
#' @param pkgdir The package directory to evaluate
#' 
#' @return A character vector of the function names
#' 
#' @noRd
get_exported_functions <- function(pkgdir) {
  ns <- parseNamespaceFile(basename(pkgdir), dirname(pkgdir))
  nsexp <- ns$exports
  nsimp <- unlist(purrr::map(ns$imports, ~ .x[-1]))
  sort(setdiff(nsexp, nsimp))
}

#' Get Parsed Data
#' 
#' Parses the files to determine which contain the function(s) of interest
#' 
#' @param type The type of files to parse
#' @param pkgdir The package directory
#' @param funcnames The list of functions to evaluate
#' 
#' @return A `tibble` object containing the type of file, file name, function,
#'   and line the function appears on
#' 
#' @noRd
#' 
#' @importFrom utils getParseData
get_parse_data <- function(type = c("test", "source"), pkgdir, funcnames = NULL) {
  type <- match.arg(type)
  dirpath <- switch (type,
    test = file.path(pkgdir, "tests", "testthat"),
    source = file.path(pkgdir, "R")
  )
  filenames <- list.files(dirpath, ".+\\.[R|r]$")
  dplyr::bind_rows(lapply(filenames, function(filename) {
    d <- parse(file.path(dirpath, filename)) %>% 
      utils::getParseData() %>% 
      dplyr::filter(token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL", "SPECIAL"))
    d <- d %>% 
      dplyr::mutate(
        type = type,
        file = filename,
        func = text,
        line = line1
      ) %>% 
      dplyr::select(type, file, func, line, token) %>% 
      dplyr::distinct()
    if (!is.null(funcnames)) {
      funcnames <- unique(c(funcnames, gsub("`", "", funcnames)))
      d <- d %>% dplyr::filter(func %in% funcnames)
    }
    d
  }))
}

#' Get test files
#' 
#' Returns the test files from the parsed data corresponding to the function of interest
#' 
#' @param funcname The name of the function to evaluate
#' @param parse_data The parsed data returns from `get_parse_data()`
#' 
#' @noRd
get_test_files <- function(funcname, parse_data) {
  func_list <- unique(c(funcname, gsub("`", "", funcname)))
  parse_data %>%
    dplyr::filter(type == "test",
                  func %in% func_list) %>% 
    dplyr::pull(file) %>% 
    unique()
}

#' Get source files
#' 
#' Returns the source files from the parsed data corresponding to the function of interest
#' 
#' @param funcname The name of the function to evaluate
#' @param parse_data The parsed data returns from `get_parse_data()`
#' 
#' @noRd
get_source_files <- function(funcname, parse_data) {
  func_list <- unique(c(funcname, gsub("`", "", funcname)))
  parse_data %>%
    dplyr::filter(type == "source", 
                  func %in% func_list) %>% 
    dplyr::pull(file) %>% 
    unique()
}

#' Get man files
#' 
#' Returns the man files from the package directory corresponding to the function of interest
#' 
#' @param funcname The name of the function to evaluate
#' @param pkgdir The package directory
#' 
#' @noRd
get_man_files <- function(funcname, pkgdir) {
  man_files <- list.files(file.path(pkgdir, "man"), ".+\\.Rd$")
  funcname_regex <- 
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", funcname) %>%
    gsub(pattern = "`", replacement = "`?") %>%
    gsub(pattern = "\\%", replacement = "\\\\\\\\\\%")
  i <- sapply(man_files, function(f) {
    s <- readLines(file.path(pkgdir, "man", f))
    any(grepl(sprintf("name\\{%s\\}|alias\\{%s\\}", funcname_regex, funcname_regex), s))
  })
  man_files[i]
}

#' Get file
#' 
#' A wrapper function to get the different types of files
#' 
#' @param funcname The name of the function to evaluate
#' @param type The type of files to retrieve
#' @param ... Additional arguments passed to the get files function type
#' 
#' @noRd
get_files <- function(funcname, type = c("test", "source", "man"), ...) {
  type <- match.arg(type)
  switch(
    type,
    test = get_test_files(funcname, ...),
    source = get_source_files(funcname, ...),
    man = get_man_files(funcname, ...)
  )
}

renderCode <- function(lines, hlindex) {
  tags$table(class = "code-table",
             tags$tbody(
               lapply(seq_along(lines), function(i) {
                 tags$tr(class = if (i %in% hlindex) "highlight" else "plain",
                         tags$td(class = "number", i),
                         tags$td(class = "code", tags$pre(class = "language-r", lines[i]))
                 )
               })
             ),
             tags$script(HTML("
        document.querySelectorAll('.code pre').forEach(bl => {
          hljs.highlightBlock(bl);
        }); 
      "))
  )
}
