get_roles_table <- function(db_name = golem::get_golem_options('assessment_db_name')) {
  dbSelect("SELECT * FROM roles", db_name = db_name) %>%
    {rownames(.) <- .$user_role; .} %>%
    dplyr::select(-id, -user_role) %>%
    t()
}