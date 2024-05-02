#' add_wgss
#'
#' @param .dataset input dataset, at individual level
#' @param col_uuid_ind the name of the column that contains the unique identifier of the individual
#' @param col_wgss a vector of the names of the variables that indicate the difficulty experienced for each of the 6 dimensions
#' @return a dataset with 7 binary columns created, wgss_any_difficulty_hh reporting households that experience a difficulty for at least one member in at least one of the 6 dimensions, and wgss_..._diff_hh for each of the 6 dimensions

aggregate_wgss <- function(.dataset = mli_indv_health, 
                           col_uuid_ind = "uuid",
                           col_wgss = c("wgss_any_difficulty",
                                        "wgss_seeing_diff", 
                                        "wgss_hearing_diff", 
                                        "wgss_walking_diff", 
                                        "wgss_remembering_diff", 
                                        "wgss_selfcare_diff", 
                                        "wgss_communicating_diff")){
  .dataset <- .dataset %>% 
    group_by(!!sym(col_uuid_ind)) %>%
    summarise(across(any_of(col_wgss), 
                     ~ case_when(sum(., na.rm=T) > 0 ~ 1, 
                                 sum(., na.rm=T) == 0 ~ 0, 
                                 sum(is.na(.))==n() ~ NA_real_),
                     .names = "{.col}_hh"))
}

