# Hot swapping the get_golem_options for a local test function
# See README_TestServer for the origin of this function
replace_get_golem_options <- function(old_server, testing_options) {
  new_server <- old_server
  body(new_server) <- as.list(body(new_server)) %>%
    append(
      rlang::expr({
        get_golem_options <- function(which) {
          test_data <- !!testing_options
          test_data[[which]]
        }
      }),
      after = 1
    ) %>%
    as.call() %>%
    as.expression()
  
  return(new_server)
}

append_command_beginning <- function(old_server, command) {
  new_server <- old_server
  
  get_test_options_func <- function(test_data) {
    return(function(which) {
      test_data[[which]]
    })
  }
  
  body(new_server) <- as.list(body(new_server)) %>%
    append(rlang::expr(get_golem_options <- !!command), after = 1) %>%
    as.call() %>%
    as.expression()
  
  return(new_server)
}